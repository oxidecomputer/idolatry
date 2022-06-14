#![no_std]

//! Runtime support types for code generated by the Idol compiler.
//!
//! Most uses of Idol don't need to pull this crate in, but generated servers
//! often do.

use core::marker::PhantomData;
use core::num::NonZeroU32;
use core::ops::Range;
use userlib::{
    sys_borrow_info, sys_borrow_read, sys_borrow_write, sys_recv, sys_reply,
    sys_reply_fault, FromPrimitive, LeaseAttributes, RecvMessage,
    ReplyFaultReason, TaskId,
};
use zerocopy::{AsBytes, FromBytes};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum ClientError {
    UnknownOperation = 0xFFFF_FE00,
    BadMessageSize = 0xFFFF_FE01,
    BadMessageContents = 0xFFFF_FE04,
    BadLease = 0xFFFF_FE02,
    ReplyBufferTooSmall = 0xFFFF_FE05,

    /// no parallel
    WentAway = 0xFFFF_FE03,
}

/// Simple return type that is used when a function can only fail due to
/// the server dying (i.e. it will never return an error code of its own)
pub struct ServerDeath;
impl From<ServerDeath> for u16 {
    fn from(_: ServerDeath) -> u16 {
        0xDEAD
    }
}
impl From<ServerDeath> for u32 {
    fn from(v: ServerDeath) -> u32 {
        u16::from(v) as u32
    }
}
impl TryFrom<u32> for ServerDeath {
    type Error = ();
    fn try_from(v: u32) -> Result<Self, Self::Error> {
        match v {
            0xDEAD => Ok(Self),
            _ => Err(()),
        }
    }
}

impl ClientError {
    pub fn into_fault(self) -> Option<ReplyFaultReason> {
        match self {
            Self::UnknownOperation => {
                Some(ReplyFaultReason::UndefinedOperation)
            }
            Self::BadMessageSize => Some(ReplyFaultReason::BadMessageSize),
            Self::BadMessageContents => {
                Some(ReplyFaultReason::BadMessageContents)
            }
            Self::BadLease => Some(ReplyFaultReason::BadLeases),
            Self::ReplyBufferTooSmall => {
                Some(ReplyFaultReason::ReplyBufferTooSmall)
            }

            // Don't fault clients that just got restarted. (Wouldn't work
            // anyway.)
            Self::WentAway => None,
        }
    }

    pub fn fail<E>(self) -> RequestError<E> {
        RequestError::Fail(self)
    }
}

impl From<ClientError> for u32 {
    fn from(x: ClientError) -> Self {
        x as u32
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
#[repr(u32)]
pub enum RequestError<E> {
    Runtime(E),
    Fail(ClientError),
}

impl<E> RequestError<E> {
    pub fn went_away() -> Self {
        Self::Fail(ClientError::WentAway)
    }

    pub fn map_runtime<T>(
        self,
        mut cvt: impl FnMut(E) -> T,
    ) -> RequestError<T> {
        match self {
            Self::Runtime(e) => RequestError::Runtime(cvt(e)),
            Self::Fail(e) => RequestError::Fail(e),
        }
    }
}

impl<E> From<E> for RequestError<E> {
    fn from(e: E) -> Self {
        Self::Runtime(e)
    }
}

/// This impl requires that E produce a u16, instead of a u32, to ensure that we
/// can zero-extend it and not conflict with any of the values of `ClientError`.
impl<E> From<RequestError<E>> for u32
where
    u16: From<E>,
{
    fn from(e: RequestError<E>) -> Self {
        match e {
            RequestError::Runtime(r) => u32::from(u16::from(r)),
            RequestError::Fail(x) => u32::from(x),
        }
    }
}

/// Trait for a server to implement if it wants to be compatible with the
/// generated dispatch loops that also route notifications.
///
/// The compiler does not generate an impl for this trait, you need to customize
/// it for your server.
pub trait NotificationHandler {
    /// Produces the notification mask that should be used on any calls to RECV.
    ///
    /// If an implementation returns 0, no notifications will be accepted.
    fn current_notification_mask(&self) -> u32;

    /// Entry point for processing notifications.
    ///
    /// The dispatch loop calls this routine when RECV has returned
    /// notifications instead of a message. The notification bits that were
    /// observed to be set (and atomically cleared) are passed as `bits`.
    fn handle_notification(&mut self, bits: u32);
}

/// Trait implemented by enums that model the operations in an IPC interface.
///
/// Normally the compiler will generate both the enum and the impl of this trait
/// for it.
pub trait ServerOp: FromPrimitive + Copy {
    /// Size of response buffer required for this message, in bytes.
    fn max_reply_size(self) -> usize;
    /// Number of leases required for this operation.
    fn required_lease_count(self) -> usize;
}

/// Trait implemented by things that serve.
///
/// The generated compiler support code will provide a blanket dispatch impl for
/// this trait, you do not normally need to implement it yourself.
pub trait Server<Op: ServerOp> {
    /// Source passed to RECV to control open vs closed receive.
    fn recv_source(&self) -> Option<TaskId>;
    /// Called by the dispatch loop if the server requested a closed receive,
    /// but the task it named has died. Typically this means the server needs to
    /// update its records.
    fn closed_recv_fail(&mut self);

    /// Handles a message.
    ///
    /// For convenience, this returns a `Result`. If it returns `Err(x)`, then
    /// `x` will be sent to the sender as the response code, with a zero-byte
    /// message. If it returns `Ok(())`, it's asking the dispatch loop to
    /// consider the message handled, and the server code is responsible for
    /// calling `sys_reply` at an appropriate time.
    fn handle(
        &mut self,
        op: Op,
        incoming: &[u8],
        rm: &RecvMessage,
    ) -> Result<(), RequestError<u16>>;
}

/// Generic server dispatch routine for cases where notifications are not
/// required.
///
/// `buffer` is scratch space for incoming messages. It must be large enough to
/// accommodate any message in the IPC interface implemented by the server `S`.
/// (This would be an array sized by an associated constant, but Rust currently
/// doesn't let us do that.)
///
/// `server` is a type that implements `Server<Op>`. More specifically, the
/// implementation must be for `(PhantomData<Op>, &mut S)`. This is a bit of a
/// hack that works around overlapping impl rules. The compiler will normally
/// generate that impl for you, based on your impl of an interface-specific
/// generated trait.
///
/// If you need notifications, use `dispatch_n`.
pub fn dispatch<S, Op: ServerOp>(buffer: &mut [u8], server: &mut S)
where
    for<'a> (core::marker::PhantomData<Op>, &'a mut S): Server<Op>,
{
    let mut server = (core::marker::PhantomData, server);
    let rm = match sys_recv(buffer, 0, server.recv_source()) {
        Ok(rm) => rm,
        Err(_) => {
            server.closed_recv_fail();
            return;
        }
    };

    let op = match Op::from_u32(rm.operation) {
        Some(op) => op,
        None => {
            sys_reply_fault(rm.sender, ReplyFaultReason::UndefinedOperation);
            return;
        }
    };

    if rm.message_len > buffer.len() {
        sys_reply_fault(rm.sender, ReplyFaultReason::BadMessageSize);
        return;
    }
    if rm.response_capacity < op.max_reply_size() {
        sys_reply_fault(rm.sender, ReplyFaultReason::ReplyBufferTooSmall);
        return;
    }

    let incoming = &buffer[..rm.message_len];

    if rm.lease_count != op.required_lease_count() {
        sys_reply_fault(rm.sender, ReplyFaultReason::BadLeases);
        return;
    }

    match server.handle(op, incoming, &rm) {
        Ok(()) => {
            // stub has taken care of it.
        }
        Err(RequestError::Runtime(code)) => {
            // stub has used the convenience return for data-less errors,
            // we'll do the reply.
            sys_reply(rm.sender, code as u32, &[]);
        }
        Err(RequestError::Fail(code)) => {
            if let Some(reason) = code.into_fault() {
                sys_reply_fault(rm.sender, reason);
            } else {
                // Cases like WentAway do not merit a reply.
            }
        }
    }
}

/// Generic server dispatch routine for servers that use notifications.
///
/// `buffer` is scratch space for incoming messages. It must be large enough to
/// accommodate any message in the IPC interface implemented by the server `S`.
/// (This would be an array sized by an associated constant, but Rust currently
/// doesn't let us do that.)
///
/// `server` is a type that implements `Server<Op>`. More specifically, the
/// implementation must be for `(PhantomData<Op>, &mut S)`. This is a bit of a
/// hack that works around overlapping impl rules. The compiler will normally
/// generate that impl for you, based on your impl of an interface-specific
/// generated trait.
///
/// `server` is required to directly impl `NotificationHandler` (i.e. you must
/// write the impl yourself).
///
/// If you don't need notifications, use `dispatch`.
pub fn dispatch_n<S: NotificationHandler, Op: ServerOp>(
    buffer: &mut [u8],
    server: &mut S,
) where
    for<'a> (core::marker::PhantomData<Op>, &'a mut S): Server<Op>,
{
    let mut server = (core::marker::PhantomData, server);
    let mask = server.1.current_notification_mask();
    let rm = match sys_recv(buffer, mask, server.recv_source()) {
        Ok(rm) => rm,
        Err(_) => {
            server.closed_recv_fail();
            return;
        }
    };

    if rm.sender == TaskId::KERNEL {
        server.1.handle_notification(rm.operation);
        return;
    }

    let incoming = &buffer[..rm.message_len];

    let op = match Op::from_u32(rm.operation) {
        Some(op) => op,
        None => {
            sys_reply_fault(rm.sender, ReplyFaultReason::UndefinedOperation);
            return;
        }
    };

    if rm.message_len > buffer.len() {
        sys_reply_fault(rm.sender, ReplyFaultReason::BadMessageSize);
        return;
    }
    if rm.response_capacity < op.max_reply_size() {
        sys_reply_fault(rm.sender, ReplyFaultReason::ReplyBufferTooSmall);
        return;
    }

    match server.handle(op, incoming, &rm) {
        Ok(()) => {
            // stub has taken care of it.
        }
        Err(RequestError::Runtime(code)) => {
            // stub has used the convenience return for data-less errors,
            // we'll do the reply.
            sys_reply(rm.sender, code as u32, &[]);
        }
        Err(RequestError::Fail(code)) => {
            if let Some(reason) = code.into_fault() {
                sys_reply_fault(rm.sender, reason);
            } else {
                // Cases like WentAway do not merit a reply.
            }
        }
    }
}

/// Marker trait implemented by types that can serve as lease attribute
/// indicators.
///
/// This is not intended to be implemented by types outside this module -- it
/// exists solely to provide some user guidance on which types can appear in the
/// "attribute" position on the `Leased` type.
pub trait Attribute {}

/// Marker trait for `Attribute` types that signal read access to leased data.
pub trait AttributeRead: Attribute {}

/// Marker trait for `Attribute` types that signal write access to leased data.
pub trait AttributeWrite: Attribute {}

/// Type used to configure `Leased` for data that can only be read.
///
/// This type serves as a marker only and is never created or manipulated at
/// runtime.
pub enum R {}

impl Attribute for R {}
impl AttributeRead for R {}

/// Type used to configure `Leased` for data that can only be written.
///
/// This type serves as a marker only and is never created or manipulated at
/// runtime.
pub enum W {}

impl Attribute for W {}
impl AttributeWrite for W {}

/// Type used to configure `Leased` for data that can be both read and written.
///
/// This type serves as a marker only and is never created or manipulated at
/// runtime.
pub enum RW {}

impl Attribute for RW {}
impl AttributeRead for RW {}
impl AttributeWrite for RW {}

/// Handle to some leased data sent by a client.
///
/// `Leased<A, T>` indicates data of type `T`, held in client memory, and
/// temporarily loaned to us with attributes `A`. In practice, `A` can be `R`,
/// `W`, or `RW`, for read-only, write-only, and read-write accesses,
/// respectively.
///
/// The server dispatch routines will prepare a `Leased` handle for each lease
/// defined for an operation in the Idol file. When the server implementation
/// is invoked, it can assume the following properties of the `Leased` data:
///
/// 1. The client actually sent the corresponding lease.
/// 2. The attributes used on the lease by the client match `A`.
/// 3. The size of the leased data is correct for `T` -- either it contains
///    exactly one `T`, or, if `T` is a slice `[E]` for some type `E`, it
///    contains an integral number of values of type `E`.
///
/// If any of these checks fail, the dispatch code will return an error to the
/// client before calling into the server impl.
pub struct Leased<A: Attribute, T: ?Sized> {
    /// Source of this lease.
    lender: TaskId,
    /// Index of this lease in the lender's lease table.
    index: usize,
    /// Number of bytes leased, cached from the borrow info.
    len: usize,
    /// Marker to make type magic work.
    _marker: PhantomData<(A, *const T)>,
}

/// These functions are available on any `Leased<A, T>` independent of the
/// choice of `A` and `T`.
impl<A: Attribute, T: ?Sized> Leased<A, T> {
    /// Gets the `TaskId` of the task that's lending this data.
    pub fn lender(&self) -> TaskId {
        self.lender
    }

    /// Gets the index of this lease in the lender's lease table. This is
    /// available in case you need to do raw operations on the data, outside
    /// this API.
    pub fn lease_index(&self) -> usize {
        self.index
    }
}

/// These functions are available on any `Leased<A, T>` where `T` is `Sized`.
impl<A: Attribute, T> Leased<A, T> {
    /// Internal implementation factor for checking `Sized` T.
    fn check_sized(
        lender: TaskId,
        index: usize,
        required_atts: LeaseAttributes,
    ) -> Option<()> {
        let info = sys_borrow_info(lender, index)?;
        if !info.attributes.contains(required_atts) {
            return None;
        }
        if info.len != core::mem::size_of::<T>() {
            return None;
        }
        Some(())
    }
}

/// These functions are available on any `Leased<A, [T]>`, that is, any leased
/// slice independent of the choice of attributes.
impl<A: Attribute, T> Leased<A, [T]> {
    /// Returns the number of elements of type `T` in the leased slice.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Checks whether the leased slice is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Internal implementation factor for checking slices.
    fn check_slice(
        lender: TaskId,
        index: usize,
        required_atts: LeaseAttributes,
        max_len: Option<NonZeroU32>,
    ) -> Option<usize> {
        let info = sys_borrow_info(lender, index)?;
        if !info.attributes.contains(required_atts) {
            return None;
        }
        if info.len % core::mem::size_of::<T>() != 0 {
            return None;
        }
        let len = info.len / core::mem::size_of::<T>();

        if let Some(max_len) = max_len {
            if len > max_len.get() as usize {
                return None;
            }
        }

        Some(len)
    }
}

/// These functions are only available for read-only leased data for `Sized`
/// types, i.e. leases of a single struct or similar.
impl<T> Leased<R, T> {
    /// Creates a new `Leased` describing read-only data. This is intended to be
    /// called from the generated server stub code.
    ///
    /// This operation will perform `sys_borrow_info` to check the properties
    /// described in the docs for `Leased`. If any fail, it returns `None`.
    pub fn read_only(lender: TaskId, index: usize) -> Option<Self> {
        Self::check_sized(lender, index, LeaseAttributes::READ)?;
        Some(Self {
            lender,
            index,
            len: 1,
            _marker: PhantomData,
        })
    }
}

/// These functions are only available for read-only leased slices.
impl<T> Leased<R, [T]> {
    /// Creates a new `Leased` describing a read-only slice. This is intended to
    /// be called from the generated server stub code.
    ///
    /// This operation will perform `sys_borrow_info` to check the properties
    /// described in the docs for `Leased`. If any fail, it returns `None`.
    pub fn read_only_slice(
        lender: TaskId,
        index: usize,
        max_len: Option<NonZeroU32>,
    ) -> Option<Self> {
        let len =
            Self::check_slice(lender, index, LeaseAttributes::READ, max_len)?;
        Some(Self {
            lender,
            index,
            len,
            _marker: PhantomData,
        })
    }
}

/// These functions are only available for write-only leases of `Sized` types,
/// e.g. a single leased struct or similar.
impl<T> Leased<W, T> {
    /// Creates a new `Leased` describing write-only data. This is intended to
    /// be called from the generated server stub code.
    ///
    /// This operation will perform `sys_borrow_info` to check the properties
    /// described in the docs for `Leased`. If any fail, it returns `None`.
    pub fn write_only(lender: TaskId, index: usize) -> Option<Self> {
        Self::check_sized(lender, index, LeaseAttributes::WRITE)?;
        Some(Self {
            lender,
            index,
            len: 1,
            _marker: PhantomData,
        })
    }
}

/// These functions are only available for write-only slices.
impl<T> Leased<W, [T]> {
    /// Creates a new `Leased` describing a write-only slice. This is intended
    /// to be called from the generated server stub code.
    ///
    /// This operation will perform `sys_borrow_info` to check the properties
    /// described in the docs for `Leased`. If any fail, it returns `None`.
    pub fn write_only_slice(
        lender: TaskId,
        index: usize,
        max_len: Option<NonZeroU32>,
    ) -> Option<Self> {
        let len =
            Self::check_slice(lender, index, LeaseAttributes::WRITE, max_len)?;
        Some(Self {
            lender,
            index,
            len,
            _marker: PhantomData,
        })
    }
}

/// These functions are available on any readable lease (that is, read-only or
/// read-write) where the content type `T` is `Sized` and can be moved around by
/// naive mem-copy.
impl<A: AttributeRead, T: Sized + Copy + FromBytes + AsBytes> Leased<A, T> {
    /// Reads the leased value by copy.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `read`, this will return `None`.
    /// Otherwise, it returns `Some(value)`. It's therefore safe to treat a
    /// `None` return as aborting the request.
    pub fn read(&self) -> Option<T> {
        let mut temp = T::new_zeroed();
        let (rc, len) =
            sys_borrow_read(self.lender, self.index, 0, temp.as_bytes_mut());
        if rc != 0 || len != core::mem::size_of::<T>() {
            None
        } else {
            Some(temp)
        }
    }
}

/// These functions are available on any readable leased slice (that is,
/// read-only or read-write) where the element type `T` is `Sized` and can be
/// moved around by naive mem-copy.
impl<A: AttributeRead, T: Sized + Copy + FromBytes + AsBytes> Leased<A, [T]> {
    /// Reads a single element of the leased slice by copy.
    ///
    /// Like indexing a native slice, `index` must be less than `self.len()`, or
    /// this will panic.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `read_at`, this will return `None`.
    /// Otherwise, it returns `Some(value)`. It's therefore safe to treat a
    /// `None` return as aborting the request.
    pub fn read_at(&self, index: usize) -> Option<T> {
        assert!(index < self.len);

        let mut temp = T::new_zeroed();
        let offset = core::mem::size_of::<T>().checked_mul(index)?;
        let (rc, len) = sys_borrow_read(
            self.lender,
            self.index,
            offset,
            temp.as_bytes_mut(),
        );
        if rc != 0 || len != core::mem::size_of::<T>() {
            None
        } else {
            Some(temp)
        }
    }

    /// Reads a range of elements of the leased slice into `dest` by copy.
    ///
    /// Like indexing a native slice, `range.start` must be less than
    /// `self.len()`, and `range.end` must be less than or equal to
    /// `self.len()`, or this will panic.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `read_range`, this will return `None`.
    /// Otherwise, it returns `Some(value)`. It's therefore safe to treat a
    /// `None` return as aborting the request.
    pub fn read_range(
        &self,
        range: Range<usize>,
        dest: &mut [T],
    ) -> Result<(), ()> {
        let offset = core::mem::size_of::<T>()
            .checked_mul(range.start)
            .ok_or(())?;
        let expected_len = core::mem::size_of::<T>()
            .checked_mul(range.end - range.start)
            .ok_or(())?;

        let (rc, len) = sys_borrow_read(
            self.lender,
            self.index,
            offset,
            dest.as_bytes_mut(),
        );

        if rc != 0 || len != expected_len {
            Err(())
        } else {
            Ok(())
        }
    }
}

/// These functions are available on any writable lease (that is, write-only or
/// read-write) where the content type `T` is `Sized` and can be moved around by
/// naive mem-copy.
impl<A: AttributeWrite, T: Sized + Copy + AsBytes> Leased<A, T> {
    /// Writes the leased value by copy.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `write`, this will return `Err(())`.
    /// Otherwise, it returns `Ok(())`. It's therefore safe to treat an `Err`
    /// return as aborting the request.
    pub fn write(&self, value: T) -> Result<(), ()> {
        let (rc, len) =
            sys_borrow_write(self.lender, self.index, 0, value.as_bytes());
        if rc != 0 || len != core::mem::size_of::<T>() {
            Err(())
        } else {
            Ok(())
        }
    }
}

/// These functions are available on any writable leased slice (that is,
/// write-only or read-write) where the element type `T` is `Sized` and can be
/// moved around by naive mem-copy.
impl<A: AttributeWrite, T: Sized + Copy + AsBytes> Leased<A, [T]> {
    /// Writes a single element of the leased slice by copy.
    ///
    /// Like indexing a native slice, `index` must be less than `self.len()`, or
    /// this will panic.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `write_at`, this will return `Err(())`.
    /// Otherwise, it returns `Ok(())`. It's therefore safe to treat an `Err`
    /// return as aborting the request.
    pub fn write_at(&self, index: usize, value: T) -> Result<(), ()> {
        let offset = core::mem::size_of::<T>().checked_mul(index).ok_or(())?;
        let (rc, len) =
            sys_borrow_write(self.lender, self.index, offset, value.as_bytes());
        if rc != 0 || len != core::mem::size_of::<T>() {
            Err(())
        } else {
            Ok(())
        }
    }

    /// Writes a range of elements from `src` into the leased slice by copy.
    ///
    /// Like indexing a native slice, `range.start` must be less than
    /// `self.len()`, and `range.end` must be less than or equal to
    /// `self.len()`, or this will panic.
    ///
    /// If the lending task has been restarted between the time we checked lease
    /// attributes and the time you call `write_range`, this will return
    /// `Err(())`. Otherwise, it returns `Ok(())`. It's therefore safe to treat
    /// an `Err` return as aborting the request.
    pub fn write_range(
        &self,
        range: Range<usize>,
        src: &[T],
    ) -> Result<(), ()> {
        let offset = core::mem::size_of::<T>()
            .checked_mul(range.start)
            .ok_or(())?;
        let expected_len = core::mem::size_of::<T>()
            .checked_mul(range.end - range.start)
            .ok_or(())?;

        let (rc, len) =
            sys_borrow_write(self.lender, self.index, offset, src.as_bytes());

        if rc != 0 || len != expected_len {
            Err(())
        } else {
            Ok(())
        }
    }
}

/// A `T` that has had its length checked to be no greater than `N`.
///
/// While this type is written generically, it's only used in practice with `T`
/// being `Leased<A, [E]>`.
///
/// `LenLimit` implements `Deref` and `DerefMut`, so all the operations from the
/// type `T` are available. That is, you can generally treat it like a `Leased`.
pub struct LenLimit<T, const N: usize>(T);

impl<T, const N: usize> LenLimit<T, N> {
    /// Unwraps the `LenLimit` decorator. This is useful when you want to ensure
    /// that something passed to you is limited-length, but you then need it
    /// in its raw form now that you're confident.
    pub fn into_inner(self) -> T {
        self.0
    }
}

impl<A: Attribute, T, const N: usize> LenLimit<Leased<A, [T]>, N> {
    /// Gets the length of the slice as a `u16` if it has been previously
    /// checked to be 65535 elements or less.
    ///
    /// Note that it is possible to call this function even when `N` is too
    /// large; this is because we're missing some const generics features still
    /// that would let us assert on that. Instead, if you do this when `N` is
    /// too large, you will get a panic. When `N` is in range, the check will
    /// compile out.
    pub fn len_as_u16(this: &Self) -> u16 {
        assert!(N <= u16::MAX as usize);
        this.0.len() as u16
    }

    /// Gets the length of the slice as a `u8` if it has been previously
    /// checked to be 255 elements or less.
    ///
    /// Note that it is possible to call this function even when `N` is too
    /// large; this is because we're missing some const generics features still
    /// that would let us assert on that. Instead, if you do this when `N` is
    /// too large, you will get a panic. When `N` is in range, the check will
    /// compile out.
    pub fn len_as_u8(this: &Self) -> u8 {
        assert!(N <= u8::MAX as usize);
        this.0.len() as u8
    }
}

impl<A: Attribute, T, const N: usize> TryFrom<Leased<A, [T]>>
    for LenLimit<Leased<A, [T]>, N>
{
    type Error = ();

    fn try_from(x: Leased<A, [T]>) -> Result<Self, Self::Error> {
        if x.len() <= N {
            Ok(Self(x))
        } else {
            Err(())
        }
    }
}

impl<T, const N: usize> core::ops::Deref for LenLimit<T, N> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T, const N: usize> core::ops::DerefMut for LenLimit<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub struct LeaseBufReader<A: AttributeRead, const N: usize> {
    /// Backing lease.
    lease: Leased<A, [u8]>,
    /// How far we've read in the lease.
    pos: usize,
    /// Number of bytes we've read that are still sitting in `buf` waiting to be
    /// observed.
    buf_left: usize,
    /// Pending bytes. Specifically, the _tail_ of `buf` containing `buf_left`
    /// bytes is valid.
    buf: [u8; N],
}

impl<A: AttributeRead, const N: usize> From<Leased<A, [u8]>>
    for LeaseBufReader<A, N>
{
    fn from(lease: Leased<A, [u8]>) -> Self {
        Self {
            lease,
            pos: 0,
            buf_left: 0,
            buf: [0; N],
        }
    }
}

impl<A: AttributeRead, const N: usize> LeaseBufReader<A, N> {
    pub fn is_eof(&self) -> bool {
        self.pos == self.lease.len() && self.buf_left == 0
    }

    pub fn refill(&mut self) -> Result<(), ()> {
        if self.buf_left == 0 {
            if self.pos == self.lease.len() {
                // We've hit the end.
                return Err(());
            }

            let end = self.lease.len().min(self.pos + N);
            let chunk_size = end - self.pos;
            // Try to fill our buffer. If this fails, it means the client went
            // away, so we'll report EOF.
            self.lease
                .read_range(self.pos..end, &mut self.buf[N - chunk_size..])?;
            // Reset buffer state:
            self.pos += chunk_size;
            self.buf_left = chunk_size;
        }
        Ok(())
    }

    pub fn read(&mut self) -> Option<u8> {
        // Refill buffer if required (and possible).
        self.refill().ok()?;
        let byte = self.buf[N - self.buf_left];
        self.buf_left -= 1;
        Some(byte)
    }
}

pub struct LeaseBufWriter<A: AttributeWrite, const N: usize> {
    /// Backing lease.
    lease: Leased<A, [u8]>,
    /// How far we've written in the lease.
    pos: usize,
    /// Number of bytes we've "written" that are still sitting in `buf` waiting
    /// to be flushed.
    ///
    /// This has the range `0..N` -- if it would reach `N`, we flush and reset
    /// it.
    buf_valid: usize,
    /// Pending bytes. Specifically, the _prefix_ of `buf` containing
    /// `buf_valid` bytes is valid.
    buf: [u8; N],
}

impl<A: AttributeWrite, const N: usize> From<Leased<A, [u8]>>
    for LeaseBufWriter<A, N>
{
    fn from(lease: Leased<A, [u8]>) -> Self {
        Self {
            lease,
            pos: 0,
            buf_valid: 0,
            buf: [0; N],
        }
    }
}

impl<A: AttributeWrite, const N: usize> LeaseBufWriter<A, N> {
    pub fn is_eof(&self) -> bool {
        self.pos + self.buf_valid == self.lease.len()
    }

    pub fn write(&mut self, byte: u8) -> Result<(), ()> {
        if self.is_eof() {
            return Err(());
        }
        self.buf[self.buf_valid] = byte;
        self.buf_valid += 1;

        if self.buf_valid == N || self.is_eof() {
            self.flush()?;
        }

        Ok(())
    }

    fn flush(&mut self) -> Result<(), ()> {
        // Try to flush. If this fails, the client has gone away, so we fail
        // too. However, we zero `buf_valid` either way to maintain our
        // invariant on that field. Since client errors never recover, losing
        // data here is unavoidable.
        let n = self.buf_valid;
        self.buf_valid = 0;
        if n != 0 {
            self.lease
                .write_range(self.pos..self.pos + n, &self.buf[..n])?;
            self.pos += n;
        }
        Ok(())
    }
}

impl<A: AttributeWrite, const N: usize> Drop for LeaseBufWriter<A, N> {
    fn drop(&mut self) {
        // Ignore flush errors on drop since it means the client went away at
        // the last minute.
        self.flush().ok();
    }
}
