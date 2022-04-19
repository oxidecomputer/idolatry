// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at https://mozilla.org/MPL/2.0/.

use indexmap::{map::Entry, IndexMap};
use serde::de::{Deserialize, Deserializer, Error, MapAccess, Visitor};
use std::fmt;
use std::hash::{BuildHasher, Hash};
use std::marker::PhantomData;

// Helper function to deserialize an `IndexMap` while failing if duplicate keys
// are encountered.
//
// The implementation is heavily derived from
// #[https://docs.rs/serde_with/1.11.0/serde_with/rust/maps_duplicate_key_is_error],
// which only supports the built-in std maps (enforced by a sealed trait). We
// hard-code `IndexMap` here instead of being more generic; we also require the
// key type to implement `Debug` so we can report the duplicated key in our
// error message.
pub(crate) fn deserialize_reject_dup_keys<'de, D, K, V, S>(
    deserializer: D,
) -> Result<IndexMap<K, V, S>, D::Error>
where
    D: Deserializer<'de>,
    K: Deserialize<'de> + Hash + Eq + fmt::Debug,
    V: Deserialize<'de>,
    S: BuildHasher + Default,
{
    let visitor = MapVisitor {
        key_type: PhantomData,
        value_type: PhantomData,
        state_type: PhantomData,
    };
    deserializer.deserialize_map(visitor)
}

struct MapVisitor<K, V, S> {
    key_type: PhantomData<K>,
    value_type: PhantomData<V>,
    state_type: PhantomData<S>,
}

impl<'de, K, V, S> Visitor<'de> for MapVisitor<K, V, S>
where
    K: Deserialize<'de> + Hash + Eq + fmt::Debug,
    V: Deserialize<'de>,
    S: BuildHasher + Default,
{
    type Value = IndexMap<K, V, S>;

    fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str("a map")
    }

    fn visit_map<A>(self, mut access: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut values = match access.size_hint() {
            Some(size) => {
                Self::Value::with_capacity_and_hasher(size, S::default())
            }
            None => Self::Value::with_hasher(S::default()),
        };

        while let Some((key, value)) = access.next_entry()? {
            match values.entry(key) {
                Entry::Occupied(slot) => {
                    return Err(Error::custom(format_args!(
                        "invalid entry: found duplicate key {:?}",
                        slot.key()
                    )));
                }
                Entry::Vacant(slot) => {
                    slot.insert(value);
                }
            }
        }

        Ok(values)
    }
}
