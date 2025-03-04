extern crate criterion;

use criterion::{criterion_group, criterion_main, Criterion};

use blazepdf_lib::parser::html::BlazePdfTreeSink;
use html5ever::tendril::TendrilSink;

fn bench_large_document(c: &mut Criterion) {
    let mut big_html = String::with_capacity(10_000_000);
    big_html.push_str("<div>");
    for _ in 0..100_000 {
        big_html.push_str("<p>Test</p>");
    }
    big_html.push_str("</div>");

    c.bench_function("large_document", |b| {
        b.iter(|| {
            let sink = BlazePdfTreeSink::new();
            html5ever::parse_document(sink, Default::default()).one(big_html.to_string())
        })
    });
}

fn bench_deep_nesting(c: &mut Criterion) {
    let mut deep_html = String::new();
    for _ in 0..1000 {
        deep_html.push_str("<div>");
    }
    deep_html.push_str("Content");
    for _ in 0..1000 {
        deep_html.push_str("</div>");
    }

    c.bench_function("deep_nesting", |b| {
        b.iter(|| {
            let sink = BlazePdfTreeSink::new();
            html5ever::parse_document(sink, Default::default()).one(deep_html.to_string())
        })
    });
}

criterion_group!(benches, bench_large_document, bench_deep_nesting);
criterion_main!(benches);
