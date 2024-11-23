// The serde_json crate implements a Serializer for its own Value enum, that is
// almost exactly the same to our Value enum, so this is more or less copied
// from [serde_json](https://github.com/serde-rs/json/blob/master/src/value/ser.rs),
// also mentioned in the [serde documentation](https://serde.rs/).

use crate::{objects::Key, Value};
use serde::{
    ser::{self, Impossible, SerializeStruct},
    Serialize,
};
use std::{collections::HashMap, fmt::Display, iter::FromIterator, sync::Arc};
use thiserror::Error;

#[cfg(feature = "chrono")]
use chrono::FixedOffset;

pub struct Serializer;
pub struct KeySerializer;

/// A wrapper Duration type which allows conversion to [Value::Duration] for
/// types using automatic conversion with [serde::Serialize].
///
/// # Examples
///
/// ```
/// use cel_interpreter::{Context, Duration, Program};
/// use serde::Serialize;
///
/// #[derive(Serialize)]
/// struct MyStruct {
///     dur: Duration,
/// }
///
/// let mut context = Context::default();
///
/// // MyStruct will be implicitly serialized into the CEL appropriate types
/// context
///     .add_variable(
///         "foo",
///         MyStruct {
///             dur: chrono::Duration::hours(2).into(),
///         },
///     )
///     .unwrap();
///
/// let program = Program::compile("foo.dur == duration('2h')").unwrap();
/// let value = program.execute(&context).unwrap();
/// assert_eq!(value, true.into());
/// ```
#[cfg(feature = "chrono")]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Duration(pub chrono::Duration);

#[cfg(feature = "chrono")]
impl Duration {
    // Since serde can't natively represent durations, we serialize a special
    // newtype to indicate we want to rebuild the duration in the result, while
    // remaining compatible with most other Serializer implemenations.
    const NAME: &str = "$__cel_private_Duration";
    const STRUCT_NAME: &str = "Duration";
    const SECS_FIELD: &str = "secs";
    const NANOS_FIELD: &str = "nanos";
}

#[cfg(feature = "chrono")]
impl From<Duration> for chrono::Duration {
    fn from(value: Duration) -> Self {
        value.0
    }
}

#[cfg(feature = "chrono")]
impl From<chrono::Duration> for Duration {
    fn from(value: chrono::Duration) -> Self {
        Self(value)
    }
}

#[cfg(feature = "chrono")]
impl ser::Serialize for Duration {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        // chrono::Duration's Serialize impl isn't stable yet and relies on
        // private fields, so attempt to mimic serde's default impl for std
        // Duration.
        struct DurationProxy(chrono::Duration);
        impl Serialize for DurationProxy {
            fn serialize<S: ser::Serializer>(
                &self,
                serializer: S,
            ) -> std::result::Result<S::Ok, S::Error> {
                let mut s = serializer.serialize_struct(Duration::STRUCT_NAME, 2)?;
                s.serialize_field(Duration::SECS_FIELD, &self.0.num_seconds())?;
                s.serialize_field(Duration::NANOS_FIELD, &self.0.subsec_nanos())?;
                s.end()
            }
        }
        serializer.serialize_newtype_struct(Self::NAME, &DurationProxy(self.0))
    }
}

/// A wrapper Timestamp type which allows conversion to [Value::Timestamp] for
/// types using automatic conversion with [serde::Serialize].
///
/// # Examples
///
/// ```
/// use cel_interpreter::{Context, Timestamp, Program};
/// use serde::Serialize;
///
/// #[derive(Serialize)]
/// struct MyStruct {
///     ts: Timestamp,
/// }
///
/// let mut context = Context::default();
///
/// // MyStruct will be implicitly serialized into the CEL appropriate types
/// context
///     .add_variable(
///         "foo",
///         MyStruct {
///             ts: chrono::DateTime::parse_from_rfc3339("2025-01-01T00:00:00Z")
///                 .unwrap()
///                 .into(),
///         },
///     )
///     .unwrap();
///
/// let program = Program::compile("foo.ts == timestamp('2025-01-01T00:00:00Z')").unwrap();
/// let value = program.execute(&context).unwrap();
/// assert_eq!(value, true.into());
/// ```
#[cfg(feature = "chrono")]
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct Timestamp(pub chrono::DateTime<FixedOffset>);

#[cfg(feature = "chrono")]
impl Timestamp {
    // Since serde can't natively represent timestamps, we serialize a special
    // newtype to indicate we want to rebuild the timestamp in the result,
    // while remaining compatible with most other Serializer implemenations.
    const NAME: &str = "$__cel_private_Timestamp";
}

#[cfg(feature = "chrono")]
impl From<Timestamp> for chrono::DateTime<FixedOffset> {
    fn from(value: Timestamp) -> Self {
        value.0
    }
}

#[cfg(feature = "chrono")]
impl From<chrono::DateTime<FixedOffset>> for Timestamp {
    fn from(value: chrono::DateTime<FixedOffset>) -> Self {
        Self(value)
    }
}

#[cfg(feature = "chrono")]
impl ser::Serialize for Timestamp {
    fn serialize<S>(&self, serializer: S) -> std::result::Result<S::Ok, S::Error>
    where
        S: ser::Serializer,
    {
        serializer.serialize_newtype_struct(Self::NAME, &self.0)
    }
}

#[derive(Error, Debug, PartialEq, Clone)]
pub enum SerializationError {
    InvalidKey(String),
    SerdeError(String),
}

impl ser::Error for SerializationError {
    fn custom<T>(msg: T) -> Self
    where
        T: std::fmt::Display,
    {
        SerializationError::SerdeError(msg.to_string())
    }
}

impl Display for SerializationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializationError::SerdeError(msg) => formatter.write_str(msg),
            SerializationError::InvalidKey(msg) => formatter.write_str(msg),
        }
    }
}

pub type Result<T> = std::result::Result<T, SerializationError>;

pub fn to_value<T>(value: T) -> Result<Value>
where
    T: Serialize,
{
    value.serialize(Serializer)
}

impl ser::Serializer for Serializer {
    type Ok = Value;
    type Error = SerializationError;

    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeVec;
    type SerializeTupleStruct = SerializeVec;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStruct = SerializeMap;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_bool(self, v: bool) -> Result<Value> {
        Ok(Value::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<Value> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Value> {
        Ok(Value::Int(v))
    }

    fn serialize_u8(self, v: u8) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<Value> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Value> {
        Ok(Value::UInt(v))
    }

    fn serialize_f32(self, v: f32) -> Result<Value> {
        self.serialize_f64(f64::from(v))
    }

    fn serialize_f64(self, v: f64) -> Result<Value> {
        Ok(Value::Float(v))
    }

    fn serialize_char(self, v: char) -> Result<Value> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Value> {
        Ok(Value::String(Arc::new(v.to_string())))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Value> {
        Ok(Value::Bytes(Arc::new(v.to_vec())))
    }

    fn serialize_none(self) -> Result<Value> {
        self.serialize_unit()
    }

    fn serialize_some<T>(self, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Value> {
        Ok(Value::Null)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Value> {
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(self, name: &'static str, value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        match name {
            #[cfg(feature = "chrono")]
            Duration::NAME => value.serialize(TimeSerializer::Duration),
            #[cfg(feature = "chrono")]
            Timestamp::NAME => value.serialize(TimeSerializer::Timestamp),
            _ => value.serialize(self),
        }
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        Ok(HashMap::from_iter([(variant.to_string(), value.serialize(Serializer)?)]).into())
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Ok(SerializeVec {
            vec: Vec::with_capacity(_len.unwrap_or(0)),
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        self.serialize_seq(Some(len))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Ok(SerializeTupleVariant {
            name: String::from(variant),
            vec: Vec::with_capacity(_len),
        })
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Ok(SerializeMap {
            map: HashMap::new(),
            next_key: None,
        })
    }

    fn serialize_struct(self, _name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        self.serialize_map(Some(len))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Ok(SerializeStructVariant {
            name: String::from(variant),
            map: HashMap::new(),
        })
    }
}

pub struct SerializeVec {
    vec: Vec<Value>,
}

pub struct SerializeTupleVariant {
    name: String,
    vec: Vec<Value>,
}

pub struct SerializeMap {
    map: HashMap<Key, Value>,
    next_key: Option<Key>,
}

pub struct SerializeStructVariant {
    name: String,
    map: HashMap<Key, Value>,
}

#[cfg(feature = "chrono")]
#[derive(Debug, Default)]
struct SerializeTimestamp {
    secs: i64,
    nanos: i32,
}

impl ser::SerializeSeq for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(Value::List(Arc::new(self.vec)))
    }
}

impl ser::SerializeTuple for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleStruct for SerializeVec {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeSeq::serialize_element(self, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeSeq::end(self)
    }
}

impl ser::SerializeTupleVariant for SerializeTupleVariant {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.vec.push(to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        let map = HashMap::from_iter([(self.name, Arc::new(self.vec))]);
        Ok(map.into())
    }
}

impl ser::SerializeMap for SerializeMap {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.next_key = Some(key.serialize(KeySerializer)?);
        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map.insert(
            self.next_key.clone().ok_or_else(|| {
                SerializationError::InvalidKey(
                    "serialize_value called before serialize_key".to_string(),
                )
            })?,
            value.serialize(Serializer)?,
        );
        Ok(())
    }

    fn end(self) -> Result<Value> {
        Ok(self.map.into())
    }
}

impl ser::SerializeStruct for SerializeMap {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        serde::ser::SerializeMap::serialize_entry(self, key, value)
    }

    fn end(self) -> Result<Value> {
        serde::ser::SerializeMap::end(self)
    }
}

impl ser::SerializeStructVariant for SerializeStructVariant {
    type Ok = Value;
    type Error = SerializationError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<()>
    where
        T: ?Sized + Serialize,
    {
        self.map
            .insert(key.serialize(KeySerializer)?, to_value(value)?);
        Ok(())
    }

    fn end(self) -> Result<Value> {
        let map: HashMap<String, Value> = HashMap::from_iter([(self.name, self.map.into())]);
        Ok(map.into())
    }
}

#[cfg(feature = "chrono")]
impl ser::SerializeStruct for SerializeTimestamp {
    type Ok = Value;
    type Error = SerializationError;
    fn serialize_field<T>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> std::result::Result<(), Self::Error>
    where
        T: ?Sized + Serialize,
    {
        match key {
            Duration::SECS_FIELD => {
                let Value::Int(val) = value.serialize(Serializer)? else {
                    return Err(SerializationError::SerdeError(
                        "invalid type of value in timestamp struct".to_owned(),
                    ));
                };
                self.secs = val;
                Ok(())
            }
            Duration::NANOS_FIELD => {
                let Value::Int(val) = value.serialize(Serializer)? else {
                    return Err(SerializationError::SerdeError(
                        "invalid type of value in timestamp struct".to_owned(),
                    ));
                };
                self.nanos = val.try_into().map_err(|_| {
                    SerializationError::SerdeError(
                        "timestamp struct nanos field is invalid".to_owned(),
                    )
                })?;
                Ok(())
            }
            _ => Err(SerializationError::SerdeError(
                "invalid field in duration struct".to_owned(),
            )),
        }
    }

    fn end(self) -> std::result::Result<Self::Ok, Self::Error> {
        Ok(chrono::Duration::seconds(self.secs)
            .checked_add(&chrono::Duration::nanoseconds(self.nanos.into()))
            .unwrap()
            .into())
    }
}

impl ser::Serializer for KeySerializer {
    type Ok = Key;
    type Error = SerializationError;

    type SerializeSeq = Impossible<Key, SerializationError>;
    type SerializeTuple = Impossible<Key, SerializationError>;
    type SerializeTupleStruct = Impossible<Key, SerializationError>;
    type SerializeTupleVariant = Impossible<Key, SerializationError>;
    type SerializeMap = Impossible<Key, SerializationError>;
    type SerializeStruct = Impossible<Key, SerializationError>;
    type SerializeStructVariant = Impossible<Key, SerializationError>;

    fn serialize_bool(self, v: bool) -> Result<Key> {
        Ok(Key::Bool(v))
    }

    fn serialize_i8(self, v: i8) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i16(self, v: i16) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i32(self, v: i32) -> Result<Key> {
        self.serialize_i64(i64::from(v))
    }

    fn serialize_i64(self, v: i64) -> Result<Key> {
        Ok(Key::Int(v))
    }

    fn serialize_u8(self, v: u8) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u16(self, v: u16) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u32(self, v: u32) -> Result<Key> {
        self.serialize_u64(u64::from(v))
    }

    fn serialize_u64(self, v: u64) -> Result<Key> {
        Ok(Key::Uint(v))
    }

    fn serialize_f32(self, _v: f32) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Float is not supported".to_string(),
        ))
    }

    fn serialize_f64(self, _v: f64) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Float is not supported".to_string(),
        ))
    }

    fn serialize_char(self, v: char) -> Result<Key> {
        self.serialize_str(&v.to_string())
    }

    fn serialize_str(self, v: &str) -> Result<Key> {
        Ok(Key::String(Arc::new(v.to_string())))
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Bytes are not supported".to_string(),
        ))
    }

    fn serialize_none(self) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "None is not supported".to_string(),
        ))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Null is not supported".to_string(),
        ))
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Key> {
        Err(SerializationError::InvalidKey(
            "Empty unit structs are not supported".to_string(),
        ))
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Key> {
        Ok(Key::String(Arc::new(variant.to_string())))
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, value: &T) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        value.serialize(KeySerializer)
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Key>
    where
        T: ?Sized + Serialize,
    {
        Err(SerializationError::InvalidKey(
            "Newtype variant is not supported".to_string(),
        ))
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        Err(SerializationError::InvalidKey(
            "Sequences are not supported".to_string(),
        ))
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        Err(SerializationError::InvalidKey(
            "Tuples are not supported".to_string(),
        ))
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        Err(SerializationError::InvalidKey(
            "Structs are not supported".to_string(),
        ))
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        Err(SerializationError::InvalidKey(
            "Tuple variants are not supported".to_string(),
        ))
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        Err(SerializationError::InvalidKey(
            "Map variants are not supported".to_string(),
        ))
    }

    fn serialize_struct(self, _name: &'static str, _len: usize) -> Result<Self::SerializeStruct> {
        Err(SerializationError::InvalidKey(
            "Structs are not supported".to_string(),
        ))
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        Err(SerializationError::InvalidKey(
            "Struct variants are not supported".to_string(),
        ))
    }
}

#[cfg(feature = "chrono")]
#[derive(Debug)]
enum TimeSerializer {
    Duration,
    Timestamp,
}

#[cfg(feature = "chrono")]
impl ser::Serializer for TimeSerializer {
    type Ok = Value;
    type Error = SerializationError;

    type SerializeStruct = SerializeTimestamp;

    // Should never be used, so just reuse existing.
    type SerializeSeq = SerializeVec;
    type SerializeTuple = SerializeVec;
    type SerializeTupleStruct = SerializeVec;
    type SerializeTupleVariant = SerializeTupleVariant;
    type SerializeMap = SerializeMap;
    type SerializeStructVariant = SerializeStructVariant;

    fn serialize_struct(self, name: &'static str, len: usize) -> Result<Self::SerializeStruct> {
        if !matches!(self, Self::Duration { .. }) || name != Duration::STRUCT_NAME {
            return Err(SerializationError::SerdeError(
                "expected Duration struct with Duration marker newtype struct".to_owned(),
            ));
        }
        if len != 2 {
            return Err(SerializationError::SerdeError(
                "expected Duration struct to have 2 fields".to_owned(),
            ));
        }
        Ok(SerializeTimestamp::default())
    }

    fn serialize_str(self, v: &str) -> Result<Value> {
        if !matches!(self, Self::Timestamp) {
            return Err(SerializationError::SerdeError(
                "expected Timestamp string with Timestamp marker newtype struct".to_owned(),
            ));
        }
        Ok(v.parse::<chrono::DateTime<FixedOffset>>()
            .map_err(|e| SerializationError::SerdeError(e.to_string()))?
            .into())
    }

    fn serialize_bool(self, _v: bool) -> Result<Value> {
        unreachable!()
    }

    fn serialize_i8(self, _v: i8) -> Result<Value> {
        unreachable!()
    }

    fn serialize_i16(self, _v: i16) -> Result<Value> {
        unreachable!()
    }

    fn serialize_i32(self, _v: i32) -> Result<Value> {
        unreachable!()
    }

    fn serialize_i64(self, _v: i64) -> Result<Value> {
        unreachable!()
    }

    fn serialize_u8(self, _v: u8) -> Result<Value> {
        unreachable!()
    }

    fn serialize_u16(self, _v: u16) -> Result<Value> {
        unreachable!()
    }

    fn serialize_u32(self, _v: u32) -> Result<Value> {
        unreachable!()
    }

    fn serialize_u64(self, _v: u64) -> Result<Value> {
        unreachable!()
    }

    fn serialize_f32(self, _v: f32) -> Result<Value> {
        unreachable!()
    }

    fn serialize_f64(self, _v: f64) -> Result<Value> {
        unreachable!()
    }

    fn serialize_char(self, _v: char) -> Result<Value> {
        unreachable!()
    }

    fn serialize_bytes(self, _v: &[u8]) -> Result<Value> {
        unreachable!()
    }

    fn serialize_none(self) -> Result<Value> {
        unreachable!()
    }

    fn serialize_some<T>(self, _value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn serialize_unit(self) -> Result<Value> {
        unreachable!()
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Value> {
        unreachable!()
    }

    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
    ) -> Result<Value> {
        unreachable!()
    }

    fn serialize_newtype_struct<T>(self, _name: &'static str, _value: &T) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn serialize_newtype_variant<T>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _value: &T,
    ) -> Result<Value>
    where
        T: ?Sized + Serialize,
    {
        unreachable!()
    }

    fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq> {
        unreachable!()
    }

    fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple> {
        unreachable!()
    }

    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleStruct> {
        unreachable!()
    }

    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeTupleVariant> {
        unreachable!()
    }

    fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap> {
        unreachable!()
    }

    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        _len: usize,
    ) -> Result<Self::SerializeStructVariant> {
        unreachable!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{objects::Key, to_value, Value};
    use crate::{Context, Program};
    use serde::Serialize;
    use serde_bytes::Bytes;
    use std::{collections::HashMap, iter::FromIterator, sync::Arc};

    #[cfg(feature = "chrono")]
    use super::{Duration, Timestamp};

    macro_rules! primitive_test {
        ($functionName:ident, $strValue: literal, $value: expr) => {
            #[test]
            fn $functionName() {
                let program = Program::compile($strValue).unwrap();
                let result = program.execute(&Context::default());
                assert_eq!(Value::from($value), result.unwrap());
            }
        };
    }

    primitive_test!(test_u64_zero, "0u", 0_u64);
    primitive_test!(test_i64_zero, "0", 0_i64);
    primitive_test!(test_f64_zero, "0.0", 0_f64);
    //primitive_test!(test_f64_zero, "0.", 0_f64); this test fails
    primitive_test!(test_bool_false, "false", false);
    primitive_test!(test_bool_true, "true", true);
    primitive_test!(test_string_empty, "\"\"", "");
    primitive_test!(test_string_non_empty, "\"test\"", "test");
    primitive_test!(test_byte_ones, r#"b"\001\001""#, vec!(1_u8, 1_u8));
    // primitive_test!(test_triple_double_quoted_string, #"r"""""""#, "");
    // primitive_test!(test_triple_single_quoted_string, "r''''''", "");
    primitive_test!(test_utf8_character_as_bytes, "b'ÿ'", vec!(195_u8, 191_u8));
    primitive_test!(test_ulid, "01JDCKSKY69ZQ9RQ8PWTF9ZQNK", ulid::Ulid::from_string("01JDCKSKY69ZQ9RQ8PWTF9ZQNK").unwrap());

    #[test]
    fn test_json_data_conversion() {
        #[derive(Serialize)]
        struct TestPrimitives {
            bool: bool,
            u8: u8,
            u16: u16,
            u32: u32,
            u64: u64,
            int8: i8,
            int16: i16,
            int32: i32,
            int64: i64,
            f32: f32,
            f64: f64,
            char: char,
            string: String,
            bytes: &'static Bytes,
        }

        let test = TestPrimitives {
            bool: true,
            int8: 8_i8,
            int16: 16_i16,
            int32: 32_i32,
            int64: 64_i64,
            u8: 8_u8,
            u16: 16_u16,
            u32: 32_u32,
            u64: 64_u64,
            f32: 0.32_f32,
            f64: 0.64_f64,
            char: 'a',
            string: "string".to_string(),
            bytes: Bytes::new(&[1_u8, 1_u8, 1_u8, 1_u8]),
        };

        let serialized = to_value(test).unwrap();
        let expected: Value = HashMap::from_iter([
            (Key::String(Arc::new("bool".to_string())), Value::Bool(true)),
            (Key::String(Arc::new("int8".to_string())), Value::Int(8)),
            (Key::String(Arc::new("int16".to_string())), Value::Int(16)),
            (Key::String(Arc::new("int32".to_string())), Value::Int(32)),
            (Key::String(Arc::new("int64".to_string())), Value::Int(64)),
            (Key::String(Arc::new("u8".to_string())), Value::UInt(8)),
            (Key::String(Arc::new("u16".to_string())), Value::UInt(16)),
            (Key::String(Arc::new("u32".to_string())), Value::UInt(32)),
            (Key::String(Arc::new("u64".to_string())), Value::UInt(64)),
            (
                Key::String(Arc::new("f32".to_string())),
                Value::Float(f64::from(0.32_f32)),
            ),
            (Key::String(Arc::new("f64".to_string())), Value::Float(0.64)),
            (
                Key::String(Arc::new("char".to_string())),
                Value::String(Arc::new("a".to_string())),
            ),
            (
                Key::String(Arc::new("string".to_string())),
                Value::String(Arc::new("string".to_string())),
            ),
            (
                Key::String(Arc::new("bytes".to_string())),
                Value::Bytes(Arc::new(vec![1, 1, 1, 1])),
            ),
        ])
        .into();

        // Test with CEL because iterator is not implemented for Value::Map
        let program = Program::compile(
            "expected.all(key, (has(serialized[key]) && (serialized[key] == expected[key])))",
        )
        .unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("serialized", serialized).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[derive(Serialize)]
    enum TestCompoundTypes {
        Unit,
        Newtype(u32),
        Wrapped(Option<u8>),
        Tuple(u32, u32),
        Struct {
            a: i32,
            nested: HashMap<bool, HashMap<String, Vec<String>>>,
        },
        Map(HashMap<String, &'static Bytes>),
    }
    #[test]
    fn test_unit() {
        let unit = to_value(TestCompoundTypes::Unit).unwrap();
        let expected: Value = "Unit".into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", unit).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }
    #[test]
    fn test_newtype() {
        let newtype = to_value(TestCompoundTypes::Newtype(32)).unwrap();
        let expected: Value = HashMap::from([("Newtype", Value::UInt(32))]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", newtype).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }
    #[test]
    fn test_options() {
        // Test Option serialization
        let wrapped = to_value(TestCompoundTypes::Wrapped(None)).unwrap();
        let expected: Value = HashMap::from([("Wrapped", Value::Null)]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", wrapped).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());

        let wrapped = to_value(TestCompoundTypes::Wrapped(Some(8))).unwrap();
        let expected: Value = HashMap::from([("Wrapped", Value::UInt(8))]).into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", wrapped).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[test]
    fn test_tuples() {
        // Test Tuple serialization
        let tuple = to_value(TestCompoundTypes::Tuple(12, 16)).unwrap();
        let expected: Value = HashMap::from([(
            "Tuple",
            Value::List(Arc::new(vec![12_u64.into(), 16_u64.into()])),
        )])
        .into();
        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", tuple).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into())
    }

    #[test]
    fn test_structs() {
        // Test Struct serialization
        let test_struct = TestCompoundTypes::Struct {
            a: 32_i32,
            nested: HashMap::from_iter([(
                true,
                HashMap::from_iter([(
                    "Test".to_string(),
                    vec!["a".to_string(), "b".to_string(), "c".to_string()],
                )]),
            )]),
        };
        let expected: Value = HashMap::<Key, Value>::from([(
            "Struct".into(),
            HashMap::<Key, Value>::from_iter([
                ("a".into(), 32_i32.into()),
                (
                    "nested".into(),
                    HashMap::<Key, Value>::from_iter([(
                        true.into(),
                        HashMap::<Key, Value>::from_iter([(
                            "Test".into(),
                            vec!["a".to_string(), "b".to_string(), "c".to_string()].into(),
                        )])
                        .into(),
                    )])
                    .into(),
                ),
            ])
            .into(),
        )])
        .into();
        let program = Program::compile("expected.all(key, test[key] == expected[key])").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", test_struct).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());
    }

    #[test]
    fn test_maps() {
        // Test Map serialization
        let map = to_value(TestCompoundTypes::Map(
            HashMap::<String, &'static Bytes>::from_iter([(
                "Test".to_string(),
                Bytes::new(&[0_u8, 0_u8, 0_u8, 0_u8]),
            )]),
        ))
        .unwrap();
        let expected: Value = HashMap::from([(
            "Map",
            HashMap::<Key, Value>::from_iter([(
                "Test".into(),
                Value::Bytes(Arc::new(vec![0_u8, 0_u8, 0_u8, 0_u8])),
            )]),
        )])
        .into();
        assert_eq!(map, expected)
    }

    #[cfg(feature = "chrono")]
    #[derive(Serialize)]
    struct TestTimeTypes {
        dur: Duration,
        ts: Timestamp,
    }

    #[cfg(feature = "chrono")]
    #[test]
    fn test_time_types() {
        use chrono::FixedOffset;

        let tests = to_value([
            TestTimeTypes {
                dur: chrono::Duration::milliseconds(1527).into(),
                ts: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                    .unwrap()
                    .into(),
            },
            // Let's test chrono::Duration's particular handling around math
            // and negatives and timestamps from BCE.
            TestTimeTypes {
                dur: chrono::Duration::milliseconds(-1527).into(),
                ts: "-0001-12-01T00:00:00-08:00"
                    .parse::<chrono::DateTime<FixedOffset>>()
                    .unwrap()
                    .into(),
            },
            TestTimeTypes {
                dur: (chrono::Duration::seconds(1) - chrono::Duration::nanoseconds(1000000001))
                    .into(),
                ts: chrono::DateTime::parse_from_rfc3339("0001-12-01T00:00:00+08:00")
                    .unwrap()
                    .into(),
            },
            TestTimeTypes {
                dur: (chrono::Duration::seconds(-1) + chrono::Duration::nanoseconds(1000000001))
                    .into(),
                ts: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                    .unwrap()
                    .into(),
            },
        ])
        .unwrap();
        let expected: Value = vec![
            Value::Map(
                HashMap::<_, Value>::from([
                    ("dur", chrono::Duration::milliseconds(1527).into()),
                    (
                        "ts",
                        chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                            .unwrap()
                            .into(),
                    ),
                ])
                .into(),
            ),
            Value::Map(
                HashMap::<_, Value>::from([
                    ("dur", chrono::Duration::nanoseconds(-1527000000).into()),
                    (
                        "ts",
                        "-0001-12-01T00:00:00-08:00"
                            .parse::<chrono::DateTime<FixedOffset>>()
                            .unwrap()
                            .into(),
                    ),
                ])
                .into(),
            ),
            Value::Map(
                HashMap::<_, Value>::from([
                    ("dur", chrono::Duration::nanoseconds(-1).into()),
                    (
                        "ts",
                        chrono::DateTime::parse_from_rfc3339("0001-12-01T00:00:00+08:00")
                            .unwrap()
                            .into(),
                    ),
                ])
                .into(),
            ),
            Value::Map(
                HashMap::<_, Value>::from([
                    ("dur", chrono::Duration::nanoseconds(1).into()),
                    (
                        "ts",
                        chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                            .unwrap()
                            .into(),
                    ),
                ])
                .into(),
            ),
        ]
        .into();
        assert_eq!(tests, expected);

        let program = Program::compile("test == expected").unwrap();
        let mut context = Context::default();
        context.add_variable("expected", expected).unwrap();
        context.add_variable("test", tests).unwrap();
        let value = program.execute(&context).unwrap();
        assert_eq!(value, true.into());
    }

    #[cfg(feature = "chrono")]
    #[cfg(feature = "json")]
    #[test]
    fn test_time_json() {
        use chrono::FixedOffset;

        // Test that Durations and Timestamps serialize correctly with
        // serde_json.
        let tests = [
            TestTimeTypes {
                dur: chrono::Duration::milliseconds(1527).into(),
                ts: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                    .unwrap()
                    .into(),
            },
            TestTimeTypes {
                dur: chrono::Duration::milliseconds(-1527).into(),
                ts: "-0001-12-01T00:00:00-08:00"
                    .parse::<chrono::DateTime<FixedOffset>>()
                    .unwrap()
                    .into(),
            },
            TestTimeTypes {
                dur: (chrono::Duration::seconds(1) - chrono::Duration::nanoseconds(1000000001))
                    .into(),
                ts: chrono::DateTime::parse_from_rfc3339("0001-12-01T00:00:00+08:00")
                    .unwrap()
                    .into(),
            },
            TestTimeTypes {
                dur: (chrono::Duration::seconds(-1) + chrono::Duration::nanoseconds(1000000001))
                    .into(),
                ts: chrono::DateTime::parse_from_rfc3339("1996-12-19T16:39:57-08:00")
                    .unwrap()
                    .into(),
            },
        ];

        let expect = "[\
{\"dur\":{\"secs\":1,\"nanos\":527000000},\"ts\":\"1996-12-19T16:39:57-08:00\"},\
{\"dur\":{\"secs\":-1,\"nanos\":-527000000},\"ts\":\"-0001-12-01T00:00:00-08:00\"},\
{\"dur\":{\"secs\":0,\"nanos\":-1},\"ts\":\"0001-12-01T00:00:00+08:00\"},\
{\"dur\":{\"secs\":0,\"nanos\":1},\"ts\":\"1996-12-19T16:39:57-08:00\"}\
]";
        let actual = serde_json::to_string(&tests).unwrap();
        assert_eq!(actual, expect);
    }
}
