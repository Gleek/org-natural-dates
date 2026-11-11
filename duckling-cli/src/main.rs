//! One JSON request/response per line; no server or network access.
use chrono::{DateTime, FixedOffset, NaiveDateTime};
use duckling::{
    parse, Context, DimensionKind, DimensionValue, Lang, Locale, Options, Region, TimePoint,
    TimeValue,
};
use serde_json::{json, Value};
use std::io::{self, BufRead, Write};

fn point(point: &TimePoint, zone: &FixedOffset) -> (NaiveDateTime, String) {
    match point {
        TimePoint::Naive { value, grain } => (*value, format!("{grain:?}")),
        TimePoint::Instant { value, grain } => (
            value.with_timezone(zone).naive_local(),
            format!("{grain:?}"),
        ),
    }
}

fn request(request: Value) -> Result<Value, String> {
    let text = request["text"].as_str().ok_or("Missing text")?;
    let reference = request["reference"].as_str().ok_or("Missing reference")?;
    let reference =
        DateTime::parse_from_str(reference, "%Y-%m-%dT%H:%M:%S%z").map_err(|e| e.to_string())?;
    let locale = Locale::new(Lang::EN, Some(Region::GB));
    let context = Context::new(reference, locale);
    let entities = parse(
        text,
        &locale,
        &[DimensionKind::Time],
        &context,
        &Options::default(),
    );
    let values: Vec<Value> = entities
        .iter()
        .filter_map(|entity| {
            let DimensionValue::Time(value) = &entity.value else {
                return None;
            };
            let mut result = json!({
                "body": entity.body,
                // Rust offsets are bytes; Emacs positions count characters.
                "start": text[..entity.start].chars().count(),
                "end": text[..entity.end].chars().count()
            });
            match value {
                TimeValue::Single { value, .. } => {
                    let (date, grain) = point(value, reference.offset());
                    result["date"] = json!(date.format("%Y-%m-%d").to_string());
                    result["time"] = json!(date.format("%H:%M").to_string());
                    result["grain"] = json!(grain);
                    result["kind"] = json!("single");
                }
                TimeValue::Interval { .. } => result["kind"] = json!("interval"),
            }
            Some(result)
        })
        .collect();
    Ok(json!({ "entities": values }))
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::BufWriter::new(io::stdout().lock());
    for line in stdin.lock().lines() {
        let Ok(line) = line else {
            break;
        };
        let result = serde_json::from_str(&line)
            .map_err(|e| e.to_string())
            .and_then(request)
            .unwrap_or_else(|error| json!({ "error": error }));
        if writeln!(stdout, "{result}")
            .and_then(|_| stdout.flush())
            .is_err()
        {
            break;
        }
    }
}
