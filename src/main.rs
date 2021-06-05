use some_math::polynomial::Polynomial;

fn main() {
    let p1 = Polynomial::new(vec![3.0, 4.6, 5.0]);
    let p2 = Polynomial::new(vec![0.5, 0.6]);

    println!("{}", p1 + p2 - Polynomial::zero());
}
