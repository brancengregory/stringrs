use extendr_api::prelude::*;
use rayon::prelude::*;
use regex::RegexSet;

#[extendr]
fn string_detect(s: Vec<String>, re: Vec<String>) -> Vec<bool> {
    if re.is_empty() {
        panic!("No regex provided");
    }
    let re_set = RegexSet::new(&re).unwrap();
    s.par_iter().map(|x| re_set.matches(x).matched_any()).collect()
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
  mod stringrs;
  fn string_detect;
}
