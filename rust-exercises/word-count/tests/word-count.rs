use std::collections::HashMap;

fn check_word_count(s: &str, pairs: &[(&str, u32)]) {
    // The reason for the awkward code in here is to ensure that the failure
    // message for assert_eq! is as informative as possible. A simpler
    // solution would simply check the length of the map, and then
    // check for the presence and value of each key in the given pairs vector.
    let mut m: HashMap<String, u32> = word_count::word_count(s);
    for &(k, v) in pairs.iter() {
        assert_eq!((k, m.remove(&k.to_string()).unwrap_or(0)), (k, v));
    }
    // may fail with a message that clearly shows all extra pairs in the map
    assert_eq!(m.iter().collect::<Vec<(&String, &u32)>>(), vec![]);
}

#[test]
fn test_count_one_word() {
    check_word_count("word", &[("word", 1)]);
}

#[test]

fn test_count_one_of_each() {
    check_word_count("one of each", &[("one", 1), ("of", 1), ("each", 1)]);
}

#[test]

fn test_count_multiple_occurrences() {
    check_word_count(
        "one fish two fish red fish blue fish",
        &[("one", 1), ("fish", 4), ("two", 1), ("red", 1), ("blue", 1)],
    );
}

#[test]

fn expanded_lists() {
    check_word_count("one\ntwo\nthree", &[("one", 1), ("two", 1), ("three", 1)]);
}

#[test]

fn test_include_numbers() {
    check_word_count(
        "testing 1 2 testing",
        &[("testing", 2), ("1", 1), ("2", 1)],
    );
}

#[test]

fn test_normalize_case() {
    check_word_count("go Go GO Stop stop", &[("go", 3), ("stop", 2)]);
}

#[test]
fn multiple_spaces_not_detected_as_a_word() {
    check_word_count(
        " multiple   whitespaces",
        &[("multiple", 1), ("whitespaces", 1)],
    );
}
