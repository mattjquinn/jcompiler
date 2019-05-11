/// Integration tests for the entire compiler.
extern crate jcompilerlib;
// extern crate tempfile;
//
// use std::process::Command;
// use std::str;
// use tempfile::NamedTempFile;
//
// fn compile(test_jfile: &str) -> (String, String) {
//     // First compile *without* optimizations/stripping.
//     let unopt_compile_to_path = String::from(
//         NamedTempFile::new()
//             .unwrap()
//             .path()
//             .to_str()
//             .expect("valid tempfile path"),
//     );
//     jcompilerlib::compile(
//         &format!("jlang_programs/{}", test_jfile)[..],
//         None,
//         0,
//         false,
//         false,
//         false,
//         Some(unopt_compile_to_path.clone()),
//     )
//     .expect("unoptimized compilation failed");
//     let unopt_output = Command::new(unopt_compile_to_path)
//         .output()
//         .expect("failed to execute unoptimized binary");
//     let unopt_stdout = str::from_utf8(&unopt_output.stdout).unwrap().to_owned();
//     let unopt_stderr = str::from_utf8(&unopt_output.stderr).unwrap().to_owned();
//
//     // Then compile *with* optimizations/stripping.
//     let opt_compile_to_path = String::from(
//         NamedTempFile::new()
//             .unwrap()
//             .path()
//             .to_str()
//             .expect("valid tempfile path"),
//     );
//     jcompilerlib::compile(
//         &format!("jlang_programs/{}", test_jfile)[..],
//         None,
//         3,
//         true,
//         false,
//         false,
//         Some(opt_compile_to_path.clone()),
//     )
//     .expect("optimized/stripped compilation failed");
//     let opt_output = Command::new(opt_compile_to_path)
//         .output()
//         .expect("failed to execute optimized/stripped binary");
//     let opt_stdout = str::from_utf8(&opt_output.stdout).unwrap().to_owned();
//     let opt_stderr = str::from_utf8(&opt_output.stderr).unwrap().to_owned();
//
//     // Ensure outputs of both agree.
//     assert_eq!(opt_stdout, unopt_stdout);
//     assert_eq!(opt_stderr, unopt_stderr);
//
//     // Return either set to caller for correctness assertions.
//     (opt_stdout, opt_stderr)
// }
//
// #[test]
// fn ctest_number_expr() {
//     let (stdout, stderr) = compile("ctest_number_expr.ijs");
//     assert_eq!("8\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_list_expr() {
//     let (stdout, stderr) = compile("ctest_list_expr.ijs");
//     assert_eq!(
//         "2 4 6 8 10\n1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_increment() {
//     let (stdout, stderr) = compile("ctest_monadic_increment.ijs");
//     assert_eq!("2 3 4 5 6\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_double_monadic_increment() {
//     let (stdout, stderr) = compile("ctest_double_monadic_increment.ijs");
//     assert_eq!("107 2002 70 46 90 2\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_square() {
//     let (stdout, stderr) = compile("ctest_monadic_square.ijs");
//     assert_eq!("1 4 9 16 25\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_double_monadic_square() {
//     let (stdout, stderr) = compile("ctest_double_monadic_square.ijs");
//     assert_eq!("1 16 81 256 625\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_increment_square() {
//     let (stdout, stderr) = compile("ctest_increment_square.ijs");
//     assert_eq!("2 5 10 17 26\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_additions_single_numbers() {
//     let (stdout, stderr) = compile("ctest_additions_single_numbers.ijs");
//     assert_eq!("3\n6\n10\n5\n0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_additions_lists() {
//     let (stdout, stderr) = compile("ctest_additions_lists.ijs");
//     assert_eq!("3 3\n6 6 6\n12 15 14\n5\n0 0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_products_single_numbers() {
//     let (stdout, stderr) = compile("ctest_products_single_numbers.ijs");
//     assert_eq!("2\n6\n24\n4\n0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_products_lists() {
//     let (stdout, stderr) = compile("ctest_products_lists.ijs");
//     assert_eq!("2 2\n6 6 6\n18 0 70\n4\n0 0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_mixed_adds_mults() {
//     let (stdout, stderr) = compile("ctest_mixed_adds_mults.ijs");
//     assert_eq!(
//         "170\n270\n45\n33\n7 7\n56 56\n28 28\n20 20 20\n10 10 10\n_2969 _3719\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_subtractions_single_positives() {
//     let (stdout, stderr) = compile("ctest_subtractions_single_positives.ijs");
//     assert_eq!("_1\n2\n_2\n3\n0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_subtractions_lists_positives() {
//     let (stdout, stderr) = compile("ctest_subtractions_lists_positives.ijs");
//     assert_eq!("_1 _1\n2 2 2\n8 _3 10\n0 0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_negate() {
//     let (stdout, stderr) = compile("ctest_monadic_negate.ijs");
//     assert_eq!(
//         "_5\n6\n_7\n8\n_2\n_1 _2 _3\n_5 _4\n2 2\n2\n2\n2\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_additions_lists_mixedlens_legal() {
//     let (stdout, stderr) = compile("ctest_additions_lists_mixedlens_legal.ijs");
//     assert_eq!(
//         "11 21 31\n11 21 31\n8 9\n7 8\n6 7\n0 0 0 0\n0 0 0\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_subtractions_lists_mixedlens_legal() {
//     let (stdout, stderr) = compile("ctest_subtractions_lists_mixedlens_legal.ijs");
//     assert_eq!(
//         "_9 _19 _29\n9 19 29\n2 3\n3 2\n2 3\n0 0 0 0\n0 0 0\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_products_lists_mixedlens_legal() {
//     let (stdout, stderr) = compile("ctest_products_lists_mixedlens_legal.ijs");
//     assert_eq!(
//         "40 80 120\n40 80 120\n12 24\n8 12\n6 8\n0 0 0 0\n0 0 0\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_insertions_plus() {
//     let (stdout, stderr) = compile("ctest_insertions_plus.ijs");
//     assert_eq!("5\n3\n6\n11\n2\n_26\n_26\n_10\n90 180 270\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_insertions_times() {
//     let (stdout, stderr) = compile("ctest_insertions_times.ijs");
//     assert_eq!(
//         "0\n5\n1\n2\n6\n30\n2\n_332\n5\n_10\n34 44 54\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_insertions_minus() {
//     let (stdout, stderr) = compile("ctest_insertions_minus.ijs");
//     assert_eq!("0\n5\n1\n_1\n2\n3\n16\n_3\n14\n10\n13 23 33\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_lessthan() {
//     let (stdout, stderr) = compile("ctest_lessthan.ijs");
//     assert_eq!(
//         "0\n1\n0 0 0 1\n1 1 0 0\n0 1 0 1\n0 0 0 0 0 1 1 1 1 1 1 1 1\n1 1 1 1 0 0 0 0 0 0 0 0 0\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_equal() {
//     let (stdout, stderr) = compile("ctest_equal.ijs");
//     assert_eq!("0\n1\n0 0 1\n1 1 1\n0 0 0\n1 1 1\n0 0 0 0 1 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 1 0 0 0 0 0 0 0 0 0 0\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_largerthan() {
//     let (stdout, stderr) = compile("ctest_largerthan.ijs");
//     assert_eq!(
//         "1\n0\n1 1 0 0\n0 0 0 1\n0 0 1 0\n1 1 1 1 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 1 1 1 1 1 1 1 1\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_is_verb_globalassgmts() {
//     let (stdout, stderr) = compile("ctest_is_verb_globalassgmts.ijs");
//     assert_eq!("", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_global_assgmts_refs() {
//     let (stdout, stderr) = compile("ctest_global_assgmts_refs.ijs");
//     assert_eq!(
//         "99\n99\n8\n9\n100\n99\n0\n1\n2\n3\n4\n5\n7.5\n1 2 3 4 5\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_global_refs_mixedverbs() {
//     let (stdout, stderr) = compile("ctest_global_refs_mixedverbs.ijs");
//     assert_eq!(
//         "1 1 0 1\n0\n3\n5 4 1 9\n1 1 1 1\n4\n6 5 2 10\n8 7 4 12\n36 25 4 100\n4 5\n5 6\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_negative_numbers() {
//     let (stdout, stderr) = compile("ctest_negative_numbers.ijs");
//     assert_eq!("_1 _2 _3 _4 _5 _6 _7 _8 _9 _10\n2\n1 1 1 0 0\n_2 _4 _6 _8\n0 1 2 3 4\n4 1 0 1 4\n_8 _4 0 4 8\n10 8 _4 3 _9\n_8640\n_8\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_decimals() {
//     let (stdout, stderr) = compile("ctest_decimals.ijs");
//     assert_eq!("0 0 0 0 0\n0 0 0 0 0\n1 1 1 1 1\n_1 _1 _1 _1 _1\n1 _2.943 3 _4 5 _800.123\n_1 _2.4 _3 0 1.9 8 7.123 2.71828\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_divisions() {
//     let (stdout, stderr) = compile("ctest_divisions.ijs");
//     let expected = format!(
//         "{}{}{}{}",
//         "0.75\n1.33333\n3 1.5 1 0.75 0.6 0.5 0.428571 0.375 0.333333 0.3\n_0.142857 0 0.142857 0.285714 0.428571 0.571429 0.714286 0.857143 1 1.14286 1.28571 1.42857\n_0.00341297 _11.9375 _0.00900901 _14.9545 _99 0.186047 1.5 _0.00121095\n",
//         "0.785375\n1.27328\n3.1415 1.57075 1.04717 0.785375 0.6283 0.523583 0.448786 0.392688 0.349056 0.31415\n0.318319 0.636639 0.954958 1.27328 1.5916 1.90992 2.22823 2.54655 2.86487 3.18319\n_2 0.601504 _0.0877174 0.00242899\n_0.5 1.6625 _11.4002 411.694\n0.729927 _0.0365297 _1.75 _0.0601142 0.403226 0.930233 0.333333 0.363636 0.166667 0.0122956 0.25\n1.37 _27.375 _0.571429 _16.635 2.48 1.075 3 2.75 6 81.33 4\n",
//         "0.174976\n_3.2085\n0.147277\n_0.00341297 _11.9375 _0.00900901 _14.9545 _99 0.186047 1.5 _0.00121095\n_293 _0.0837696 _111 _0.0668693 _0.010101 5.375 0.666667 _825.8\n",
//         "_0.609756 _2.17391 5 2.17391 1.25 0.847458 0.625 0.5\n0.97561 3.47826 _8 _3.47826 _2 _1.35593 _1 _0.8\n0 0 0 0 0 0 0 0\n_1.64 _0.46 0.2 0.46 0.8 1.18 1.6 2\n1.025 0.2875 _0.125 _0.2875 _0.5 _0.7375 _1 _1.25\n_1.01512 _3.61913 8.324 3.61913 2.081 1.41085 1.0405 0.8324\n1.11256 3.96652 _9.123 _3.96652 _2.28075 _1.54627 _1.14037 _0.9123\n_0.985103 _0.276309 0.120135 0.276309 0.480538 0.708794 0.961076 1.20135\n0.898827 0.25211 _0.109613 _0.25211 _0.438452 _0.646717 _0.876905 _1.09613\n_1.30159 0.2875 _0.111111 0.191667 2 0.120408 _0.14752 1.11112\n_0.768293 3.47826 _9 5.21739 0.5 8.30508 _6.77875 0.899991\n"
//     );
//     assert_eq!(expected, &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_power() {
//     let (stdout, stderr) = compile("ctest_power.ijs");
//     assert_eq!("1\n1\n1\n8\n_8\n59049\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_reciprocal() {
//     let (stdout, stderr) = compile("ctest_monadic_reciprocal.ijs");
//     assert_eq!("1\n0.5\n0.333333\n0.25\n_1\n_0.5\n_0.333333\n_0.25\n1 0.5 0.333333 0.25 0.2\n_1 _0.5 _0.333333 _0.25 _0.2\n2.28333\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_tally() {
//     let (stdout, stderr) = compile("ctest_monadic_tally.ijs");
//     assert_eq!("1\n2\n3\n4\n_1 2 _3 4 _5\n5\n5\n1\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_dyadic_copy() {
//     let (stdout, stderr) = compile("ctest_dyadic_copy.ijs");
//     assert_eq!("1 1 1 1 1\n5\n\n0 0 0\n_1 _1 _1 _1 _1 _1 _1\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_ceiling() {
//     let (stdout, stderr) = compile("ctest_monadic_ceiling.ijs");
//     assert_eq!(
//         "_1 1 2\n_1 _1 _1 _1 _1 0 0 0 3 4 5556\n1 2 3 4 5\n_1 1 3\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_largerof() {
//     let (stdout, stderr) = compile("ctest_monadic_largerof.ijs");
//     assert_eq!(
//         "3 3 5\n3\n3 3 3 3 4 5 6\n0 _1 _2 _3 _3 _3 _3\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_largerorequal() {
//     let (stdout, stderr) = compile("ctest_monadic_largerorequal.ijs");
//     assert_eq!(
//         "1 1 0\n1 1 1 0 0\n1\n1 1 1 1 1\n1 1 1 0 0\n1 1 0 0 0\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_ch1_learningjbook() {
//     let (stdout, stderr) = compile("ctest_ch1_learningjbook.ijs");
//     let expected = format!(
//         "{}{}{}{}{}",
//         "4\n6\n6\n0.75\n1\n_1\n_3\n8\n16\n1 4 9 16\n11 22 33\n11 21 31\n11 12 13\n",
//         "0 1 0 1 0 1 0 1\n0 1 2 0 1 2 0 1\n12\n9\n1.75\n99\n99\n8\n9\n100\n99\n",
//         "0\n1\n0.25\n9\n24\n1\n0\n0\n1 1 0 1\n0\n3\n5 4 1 9\n1 1 1 1\n4\n4\n",
//         "3 4 5\n1 1 1\n3\n3\n6 7 8 9 10\n6 7 9\n6 7 8 9 10\n",
//         "0 0 1 1 1\n8 9 10\n12\n_1 1 2\n3 3 5\n6\n6\n6\n6\n6\n6\n_1 4 6 7.3\n1 1 0\n"
//     );
//     assert_eq!(expected, &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_strings() {
//     let (stdout, stderr) = compile("ctest_strings.ijs");
//     assert_eq!("\n\nMy Ten Years in a Quandary\nA VERRRRRRRRRRRRRRRRRRRRRRRRY LOOOOOOOOOOOOONG STRINNNNNNNNNNNG\n0\n1\n2\nParty like it's 1999.\n21\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_dyadic_shape() {
//     let (stdout, stderr) = compile("ctest_dyadic_shape.ijs");
//     let expected = format!(
//         "{}{}{}{}{}{}",
//         "\n\n1\n1\n100 100 100\n_1 2 _3 4 _1\n_1 2 _3 4 5 _6 7 _8\n",
//         "1 1\n1 1\n1 2\n0 1\n1 2\n3 4\n1 2\n3 4\n10   1\n 1 100\n 10  1 9000\n100 10    1\n",
//         "  10    1 9000\n 100   10    1\n\n9000  100   10\n   1 9000  100\n\n  10    1 9000\n 100   10    1\n\n9000  100   10\n   1 9000  100\n",
//         "  50    1 8000\n 400    5   50\n\n   1 8000  400\n   5   50    1\n\n8000  400    5\n  50    1 8000\n\n\n 400    5   50\n   1 8000  400\n\n   5   50    1\n8000  400    5\n\n  50    1 8000\n 400    5   50\n",
//         "9 99 999\n9 99 999\n\n9 99 999\n9 99 999\n\n9 99 999\n9 99 999\n\n\n9 99 999\n9 99 999\n\n9 99 999\n9 99 999\n\n9 99 999\n9 99 999\n",
//         "   8   88  888\n8888    8   88\n\n 888 8888    8\n  88  888 8888\n\n   8   88  888\n8888    8   88\n\n\n 888 8888    8\n  88  888 8888\n\n   8   88  888\n8888    8   88\n\n 888 8888    8\n  88  888 8888\n\n\n\n   8   88  888\n8888    8   88\n\n 888 8888    8\n  88  888 8888\n\n   8   88  888\n8888    8   88\n\n\n 888 8888    8\n  88  888 8888\n\n   8   88  888\n8888    8   88\n\n 888 8888    8\n  88  888 8888\n"
//     );
//     assert_eq!(expected, &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_monadic_shapeof() {
//     let (stdout, stderr) = compile("ctest_monadic_shapeof.ijs");
//     assert_eq!(
//         "\n0\n3\n4\n1\n2 4\n2\n5 4 2\n3\n9 3 4 1\n4\n1 10 14 8 9\n5\n1 10 1 8 1\n5\n",
//         &stdout[..]
//     );
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_ch2_learningjbook() {
//     let (stdout, stderr) = compile("ctest_ch2_learningjbook.ijs");
//     let expected = format!(
//         "{}{}{}{}",
//         "5 6  7\n8 9 10\n5 6 7 8\n9 5 6 7\n1 1\n1 1\n50 60  70\n80 90 100\n10 12 14\n16 18 20\n0 0  0\n8 9 10\n1 1 1\n5 6 7\n5 6 7\n5 6 7\n8 5 6\n\n7 8 5\n6 7 8\n",
//         "2\n3\n3\n1 1 1\n1 1 1\n2 3\n5 6 7\n3\n1\n1 1 1\n1 1 1\n2 3\n2\n0\n99 99\n\n\n17\n17\n17\n0\n1\n2\n5\n6\n7\n3 1\n2\n",
//         "6\n\n0\n4 5 6\n3\n1\n0 1 2\n3 4 5\n2 3\n2\n0  1  2\n3  4  5\n\n6  7  8\n9 10 11\n2 2 3\n3\n",
//         "My Ten Years in a Quandary\nWhat's new?\n\n0\nrearranged\n0 1 2 3\n1 2 3 0\n0 0\n1 2 3 1 2 3\ncat\ndog\nrat\npig\ncat\ndog\nrat\npig\n1 2 3\n3\ncat\ndog\n2 3\n2\n1\ncat\ndog\nrat\npig\ncat\ndog\nrat\npig\n6\n6\n"
//     );
//     assert_eq!(expected, &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
//
// #[test]
// fn ctest_dyadic_append() {
//     let (stdout, stderr) = compile("ctest_dyadic_append.ijs");
//     assert_eq!("0 0\n_1.4 0\n_3 _8.9\n0 1 2 3 4 5\n0 1 2 3 4 5\n0 1 2 3 4 5\n_1 2 3 4 5 8 10 11 12 13 14 15\n500\n1010\n100 100 100 100 100 100 100 100 100 100\n", &stdout[..]);
//     assert_eq!("", &stderr[..]);
// }
