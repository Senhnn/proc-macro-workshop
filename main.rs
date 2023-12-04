// Write code here.
//
// To see what the code looks like after macro expansion:
//     $ cargo expand
//
// To run the code:
//     $ cargo run

use seq::seq;

seq!(N in 1..4 {
    fn f~N () -> u64 {
        N * 2
    }
});

fn f0() -> u64 {
    100
}

fn main() {
    let sum = f0() + f1() + f2() + f3();

    assert_eq!(sum, 100 + 2 + 4 + 6);
}

