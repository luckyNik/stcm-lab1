use rand::Rng;

const BIG_INT_LIMBS: usize = 32;

enum Comparison {
    Greater,
    Equal,
    Less,
}

// little-endian; implements a number with LIMBS * 64 binary digits
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BigIntTempl<const LIMBS: usize> {
    limbs: [u64; LIMBS],
}

pub type BigInt = BigIntTempl<BIG_INT_LIMBS>;

fn u64_to_big_int(value: u64) -> BigInt {
    let mut limbs = [0; BIG_INT_LIMBS];
    limbs[0] = value;

    BigInt { limbs }
}

fn hex_to_big_int(hex: &str) -> Option<BigInt> {
    let mut result_limbs = [0; BIG_INT_LIMBS];
    if hex.len() > BIG_INT_LIMBS * 16 {
        return None;
    }

    let full_groups = hex.len() / 16;
    let remainder = hex.len() % 16;
    let bytes = hex.as_bytes();

    let hex_value = |byte| -> Option<u64> {
        Some(match byte {
            b'0' => 0,
            b'1' => 1,
            b'2' => 2,
            b'3' => 3,
            b'4' => 4,
            b'5' => 5,
            b'6' => 6,
            b'7' => 7,
            b'8' => 8,
            b'9' => 9,
            b'A' | b'a' => 10,
            b'B' | b'b' => 11,
            b'C' | b'c' => 12,
            b'D' | b'd' => 13,
            b'E' | b'e' => 14,
            b'F' | b'f' => 15,
            _ => return None,
        })
    };

    for limb in 0..full_groups {
        let end = bytes.len() - limb * 16;
        for digit in 0..16 {
            let value = match hex_value(bytes[end - 1 - digit]) {
                Some(v) => v,
                None => return None,
            };
            result_limbs[limb] |= value << (digit * 4);
        }
    }

    for digit in 0..remainder {
        let value = match hex_value(bytes[remainder - 1 - digit]) {
            Some(v) => v,
            None => return None,
        };
        result_limbs[full_groups] |= value << (digit * 4);
    }

    Some(BigInt {
        limbs: result_limbs,
    })
}

fn big_int_to_hex(number: &BigInt) -> String {
    const DIGITS: &[u8; 16] = b"0123456789ABCDEF";
    let mut bytes = Vec::with_capacity(BIG_INT_LIMBS * 16);

    for limb in (0..BIG_INT_LIMBS).rev() {
        for digit in (0..16).rev() {
            let value = (number.limbs[limb] >> (digit * 4)) & 0xF;
            bytes.push(DIGITS[value as usize]);
        }
    }

    let first = bytes
        .iter()
        .position(|&b| b != b'0')
        .unwrap_or(bytes.len() - 1);
    String::from_utf8(bytes[first..].to_vec()).unwrap()
}

fn compare_big_int(a: &BigInt, b: &BigInt) -> Comparison {
    let mut i = BIG_INT_LIMBS - 1;
    while a.limbs[i] == b.limbs[i] {
        if i == 0 {
            return Comparison::Equal;
        }
        i -= 1;
    }

    if a.limbs[i] > b.limbs[i] {
        return Comparison::Greater;
    } else {
        return Comparison::Less;
    }
}

fn add_big_int(a: &BigInt, b: &BigInt) -> BigInt {
    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    let mut carry = 0u64;

    for i in 0..BIG_INT_LIMBS {
        let temp = a.limbs[i] as u128 + b.limbs[i] as u128 + carry as u128;
        result.limbs[i] = temp as u64;
        carry = (temp >> 64) as u64;
    }
    result
}

fn subtract_big_int(a: &BigInt, b: &BigInt) -> Option<BigInt> {
    if matches!(compare_big_int(a, b), Comparison::Less) {
        return None;
    }

    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    let mut borrow = false;

    for i in 0..BIG_INT_LIMBS {
        let (mut difference, mut next_borrow) = if a.limbs[i] >= b.limbs[i] {
            (a.limbs[i] - b.limbs[i], false)
        } else {
            (u64::MAX - b.limbs[i] + a.limbs[i] + 1, true)
        };

        if borrow {
            if difference == 0 {
                difference = u64::MAX;
                next_borrow = true;
            } else {
                difference -= 1;
            }
        }

        result.limbs[i] = difference;
        borrow = next_borrow;
    }
    Some(result)
}

fn shift_big_int_significant_limbs(a: &mut BigInt, limb_count: usize) {
    if limb_count == 0 {
        return;
    }

    for i in (0..BIG_INT_LIMBS).rev() {
        a.limbs[i] = if i >= limb_count {
            a.limbs[i - limb_count]
        } else {
            0
        };
    }
}

fn shift_big_int_significant_bits(a: &mut BigInt, bit_count: usize) {
    let limb_shift = bit_count / 64;
    let bit_shift = bit_count % 64;

    for i in (0..BIG_INT_LIMBS).rev() {
        if i < limb_shift {
            a.limbs[i] = 0;
            continue;
        }

        let source = i - limb_shift;
        let mut shifted = a.limbs[source] << bit_shift;
        if bit_shift != 0 && source > 0 {
            shifted |= a.limbs[source - 1] >> (64 - bit_shift);
        }
        a.limbs[i] = shifted;
    }
}

fn mult_big_int_digit(a: &BigInt, digit: u64) -> BigInt {
    let mut carry = 0;
    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };

    for i in 0..BIG_INT_LIMBS {
        let temp = a.limbs[i] as u128 * digit as u128 + carry;
        result.limbs[i] = temp as u64;
        carry = temp >> 64;
    }
    result
}

fn mult_big_int(a: &BigInt, b: &BigInt) -> BigInt {
    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    for i in 0..BIG_INT_LIMBS {
        let mut temp = mult_big_int_digit(a, b.limbs[i]);
        shift_big_int_significant_limbs(&mut temp, i);
        result = add_big_int(&result, &temp)
    }
    result
}

fn square_big_int(a: &BigInt) -> BigInt {
    let mut cross_terms = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };

    for i in 0..BIG_INT_LIMBS {
        let mut carry = 0u128;
        let mut j = i + 1;
        while i + j < BIG_INT_LIMBS {
            let product = a.limbs[i] as u128 * a.limbs[j] as u128;
            let low_sum = cross_terms.limbs[i + j] as u128
                + (product as u64 as u128) * 2
                + (carry as u64 as u128);
            cross_terms.limbs[i + j] = low_sum as u64;
            carry = (product >> 64) * 2 + (carry >> 64) + (low_sum >> 64);
            j += 1;
        }
    }

    let mut diagonal = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    for i in 0..(BIG_INT_LIMBS / 2) {
        let product = a.limbs[i] as u128 * a.limbs[i] as u128;
        diagonal.limbs[2 * i] = product as u64;
        diagonal.limbs[2 * i + 1] = (product >> 64) as u64;
    }

    add_big_int(&cross_terms, &diagonal)
}

//returns number of nonzero bits
fn bit_length_big_int(a: &BigInt) -> usize {
    for i in (0..BIG_INT_LIMBS).rev() {
        let bits = a.limbs[i];
        if bits != 0 {
            return i * 64 + (64 - bits.leading_zeros() as usize);
        }
    }
    0
}

fn divide_mod_big_int(a: &BigInt, b: &BigInt) -> Option<(BigInt, BigInt)> {
    if b == &u64_to_big_int(0u64) {
        return None;
    }

    let b_len = bit_length_big_int(b);
    let mut reminder = a.clone();
    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    while !matches!(compare_big_int(&reminder, b), Comparison::Less) {
        let mut t = bit_length_big_int(&reminder);
        let mut candidate = BigInt { limbs: b.limbs };
        shift_big_int_significant_bits(&mut candidate, t - b_len);
        if matches!(compare_big_int(&reminder, &candidate), Comparison::Less) {
            t -= 1;
            candidate = BigInt { limbs: b.limbs };
            shift_big_int_significant_bits(&mut candidate, t - b_len);
        }
        reminder = subtract_big_int(&reminder, &candidate).unwrap();
        result.limbs[(t - b_len) / 64] |= 1 << ((t - b_len) % 64);
    }
    Some((result, reminder))
}

fn horner_exponentiation_big_int(a: &BigInt, b: &BigInt) -> BigInt {
    let mut result = BigInt {
        limbs: [0u64; BIG_INT_LIMBS],
    };
    result.limbs[0] = 1;

    let iterations = bit_length_big_int(b);
    for i in (1..iterations).rev() {
        if b.limbs[i / 64] & (1u64 << (i % 64)) != 0 {
            result = mult_big_int(&result, &a);
        }
        result = square_big_int(&result);
    }
    if (b.limbs[0] & 1u64) == 1 {
        result = mult_big_int(&result, &a);
    }
    result
}

struct BenchmarkInput {
    a: BigInt,
    b: BigInt,
    exponent: BigInt,
}

fn main() {
    let mut rng = rand::rng();
    let mut inputs = Vec::with_capacity(10_000);
    let zero = u64_to_big_int(0);

    for _ in 0..10_000 {
        let a = BigInt {
            limbs: std::array::from_fn(|_| rng.random()),
        };
        let mut b = BigInt {
            limbs: std::array::from_fn(|_| rng.random()),
        };
        if b == zero {
            b.limbs[0] = 1;
        }
        let exponent = u64_to_big_int(rng.random_range(1..=1023));
        inputs.push(BenchmarkInput { a, b, exponent });
    }

    for input in &inputs[..1_000] {
        std::hint::black_box(add_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(add_big_int(&input.a, &input.b));
    }
    let add_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Addition\n{add_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(subtract_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(subtract_big_int(&input.a, &input.b));
    }
    let subtract_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Subtraction\n{subtract_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(compare_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(compare_big_int(&input.a, &input.b));
    }
    let compare_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Comparison\n{compare_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(mult_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(mult_big_int(&input.a, &input.b));
    }
    let multiply_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Multiplication\n{multiply_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(square_big_int(&input.a));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(square_big_int(&input.a));
    }
    let square_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Square\n{square_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(divide_mod_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(divide_mod_big_int(&input.a, &input.b));
    }
    let divide_mod_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Division and modulo\n{divide_mod_average:.1} ns/op\n");

    for input in &inputs[..1_000] {
        std::hint::black_box(horner_exponentiation_big_int(&input.a, &input.exponent));
    }
    let started = std::time::Instant::now();
    for input in &inputs {
        std::hint::black_box(horner_exponentiation_big_int(&input.a, &input.exponent));
    }
    let exponentiation_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Horner exponentiation\n{exponentiation_average:.1} ns/op\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_hex_to_big_int_single() {
        let s = "123A";

        let mut limbs = [0u64; BIG_INT_LIMBS];
        limbs[0] = 0x123A;
        let expected = BigInt { limbs };

        assert_eq!(hex_to_big_int(s).unwrap(), expected);
    }

    #[test]
    fn test_hex_to_big_int_multiple() {
        let s = "1000000000000123A";

        let mut limbs = [0u64; BIG_INT_LIMBS];
        limbs[0] = 0x123A;
        limbs[1] = 0x1;
        let expected = BigInt { limbs };

        assert_eq!(hex_to_big_int(s).unwrap(), expected);
    }

    #[test]
    fn test_big_int_to_hex_single() {
        let mut limbs = [0u64; BIG_INT_LIMBS];
        limbs[0] = 0x123A;
        let number = BigInt { limbs };

        assert_eq!(big_int_to_hex(&number), "123A");
    }

    #[test]
    fn test_big_int_to_hex_multiple() {
        let mut limbs = [0u64; BIG_INT_LIMBS];
        limbs[0] = 0x123A;
        limbs[1] = 0x1;
        let number = BigInt { limbs };

        assert_eq!(big_int_to_hex(&number), "1000000000000123A");
    }

    #[test]
    fn test_u64_to_big_int_zero() {
        let expected = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };

        assert_eq!(u64_to_big_int(0), expected);
    }

    #[test]
    fn test_u64_to_big_int_value() {
        let mut limbs = [0; BIG_INT_LIMBS];
        limbs[0] = 91726;
        let expected = BigInt { limbs };

        assert_eq!(u64_to_big_int(91726), expected);
    }

    #[test]
    fn test_cmp_equal() {
        let mut a = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        a.limbs[0] = 7;
        let mut b = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        b.limbs[0] = 7;

        assert!(matches!(compare_big_int(&a, &b), Comparison::Equal))
    }

    #[test]
    fn test_cmp_less() {
        let mut a = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        a.limbs[0] = 6;
        let mut b = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        b.limbs[0] = 7;

        assert!(matches!(compare_big_int(&a, &b), Comparison::Less))
    }

    #[test]
    fn test_cmp_greater() {
        let mut a = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        a.limbs[0] = 7;
        let mut b = BigInt {
            limbs: [0u64; BIG_INT_LIMBS],
        };
        b.limbs[0] = 6;

        assert!(matches!(compare_big_int(&a, &b), Comparison::Greater))
    }

    #[test]
    fn test_add_big_int_carry() {
        let mut a_limbs = [0; BIG_INT_LIMBS];
        a_limbs[0] = u64::MAX;
        a_limbs[1] = 4;
        let a = BigInt { limbs: a_limbs };

        let mut b_limbs = [0; BIG_INT_LIMBS];
        b_limbs[0] = 1;
        b_limbs[1] = 2;
        let b = BigInt { limbs: b_limbs };

        let mut expected_limbs = [0; BIG_INT_LIMBS];
        expected_limbs[1] = 7;
        let expected = BigInt {
            limbs: expected_limbs,
        };

        assert_eq!(add_big_int(&a, &b), expected);
    }

    #[test]
    fn test_subtract_success() {
        let mut a_limbs = [0; BIG_INT_LIMBS];
        a_limbs[1] = 1;
        let a = BigInt { limbs: a_limbs };

        let mut b_limbs = [0; BIG_INT_LIMBS];
        b_limbs[0] = 1;
        let b = BigInt { limbs: b_limbs };

        let mut expected_limbs = [0; BIG_INT_LIMBS];
        expected_limbs[0] = u64::MAX;
        let expected = BigInt {
            limbs: expected_limbs,
        };

        assert_eq!(subtract_big_int(&a, &b).unwrap(), expected);
    }

    #[test]
    fn test_subtract_fail() {
        let mut a_limbs = [0; BIG_INT_LIMBS];
        a_limbs[1] = 1;
        let a = BigInt { limbs: a_limbs };

        let mut b_limbs = [0; BIG_INT_LIMBS];
        b_limbs[0] = 1;
        let b = BigInt { limbs: b_limbs };

        assert_eq!(subtract_big_int(&b, &a), None);
    }

    #[test]
    fn test_subtract_self() {
        let mut a_limbs = [0; BIG_INT_LIMBS];
        a_limbs[1] = 1;
        let a = BigInt { limbs: a_limbs };

        let expected_limbs = [0; BIG_INT_LIMBS];
        let expected = BigInt {
            limbs: expected_limbs,
        };

        assert_eq!(subtract_big_int(&a, &a).unwrap(), expected);
    }

    #[test]
    fn test_divide_mod_by_zero() {
        assert_eq!(
            divide_mod_big_int(&u64_to_big_int(10), &u64_to_big_int(0)),
            None
        );
    }

    #[test]
    fn test_divide_mod_larger_by_smaller() {
        assert_eq!(
            divide_mod_big_int(
                &hex_to_big_int(
                    "AABABABABABABABABABABAABABABAAABABABABABABABABABABABABABABABABAAABABAB"
                )
                .unwrap(),
                &hex_to_big_int("34234534645345324534564635345643563456343456").unwrap()
            ),
            Some((
                hex_to_big_int("3464AFF4F01C1CA3B3320E2D812").unwrap(),
                hex_to_big_int("1C29AEB03271A65F72AF3C44D7B4CEE6937E9AEB6D9F").unwrap()
            ))
        );
    }

    #[test]
    fn test_divide_mod_smaller_by_larger() {
        assert_eq!(
            divide_mod_big_int(
                &hex_to_big_int("34234534645345324534564635345643563456343456").unwrap(),
                &hex_to_big_int(
                    "AABABABABABABABABABABAABABABAAABABABABABABABABABABABABABABABABAAABABAB"
                )
                .unwrap(),
            ),
            Some((
                u64_to_big_int(0),
                hex_to_big_int("34234534645345324534564635345643563456343456").unwrap(),
            ))
        );
    }

    #[test]
    fn test_shift_big_int_significant_limbs() {
        let mut a = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        a.limbs[0] = 5;
        a.limbs[1] = 7;

        shift_big_int_significant_limbs(&mut a, 1);

        let mut expected = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        expected.limbs[1] = 5;
        expected.limbs[2] = 7;
        assert_eq!(a, expected);
    }

    #[test]
    fn test_shift_big_int_significant_bits() {
        let mut a = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        a.limbs[0] = 0x8000_0000_0000_0001;

        shift_big_int_significant_bits(&mut a, 1);

        let mut expected = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        expected.limbs[0] = 2;
        expected.limbs[1] = 1;
        assert_eq!(a, expected);
    }

    #[test]
    fn test_mult_big_int_digit_normal() {
        let a = u64_to_big_int(12_345);
        let expected = u64_to_big_int(15_233_730);

        assert_eq!(mult_big_int_digit(&a, 1_234), expected);
    }

    #[test]
    fn test_mult_big_int_digit_zero() {
        let mut limbs = [0; BIG_INT_LIMBS];
        limbs[0] = u64::MAX;
        limbs[BIG_INT_LIMBS - 1] = u64::MAX;
        let a = BigInt { limbs };

        assert_eq!(mult_big_int_digit(&a, 0), u64_to_big_int(0));
    }

    #[test]
    fn test_mult_big_int_digit_overflow() {
        let a = BigInt {
            limbs: [u64::MAX; BIG_INT_LIMBS],
        };
        let mut expected_limbs = [u64::MAX; BIG_INT_LIMBS];
        expected_limbs[0] = u64::MAX - 1;
        let expected = BigInt {
            limbs: expected_limbs,
        };

        assert_eq!(mult_big_int_digit(&a, 2), expected);
    }

    #[test]
    fn test_mult_big_int() {
        assert_eq!(
            mult_big_int(
                &hex_to_big_int("ABCDEFEFFDFDDBCBDBAAAAACEEEEEEFEEFE").unwrap(),
                &hex_to_big_int("BABFAFBAEBAFEAFEAEF").unwrap()
            ),
            hex_to_big_int("7D5444E99B6A6FD5AAA81359C0B5FFAC76C7A80AD34528C6164B22").unwrap()
        )
    }

    #[test]
    fn test_mult_distributive_over_addition() {
        let a = hex_to_big_int("FEDCBA9876543210FFFFFFFFFFFFFFFF").unwrap();
        let b = hex_to_big_int("123456789ABCDEF0123456789ABCDEF0").unwrap();
        let c = hex_to_big_int("10000000000000001").unwrap();
        let sum = add_big_int(&a, &b);

        assert_eq!(
            mult_big_int(&sum, &c),
            add_big_int(&mult_big_int(&a, &c), &mult_big_int(&b, &c))
        );
    }

    #[test]
    fn test_mult_by_repeated_addition() {
        let multiplier = u64_to_big_int(100);
        let value = hex_to_big_int("FFFFFFFFFFFFFFFF0000000000000001").unwrap();
        let mut repeated_sum = u64_to_big_int(0);

        for _ in 0..100 {
            repeated_sum = add_big_int(&repeated_sum, &value);
        }

        assert_eq!(mult_big_int(&multiplier, &value), repeated_sum);
    }

    #[test]
    fn test_mult_commutative() {
        let a = hex_to_big_int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF").unwrap();
        let b = hex_to_big_int("10000000000000001").unwrap();

        assert_eq!(mult_big_int(&a, &b), mult_big_int(&b, &a));
    }

    #[test]
    fn test_square_big_int() {
        assert_eq!(
            square_big_int(&hex_to_big_int("ABCDEFEFFDFDDBCBDBAAAAACEEEEEEFEEFE").unwrap()),
            hex_to_big_int(
                "734CC434C48FE970985A9A52515963D6E4643A2B2A625BDE8BBCB60B1C298865614404"
            )
            .unwrap()
        )
    }

    #[test]
    fn test_square_mult_consistency() {
        let value = hex_to_big_int("ABCDEFEFFDFDDBCBDBAAAAACEEEEEEFEEFE").unwrap();

        assert_eq!(square_big_int(&value), mult_big_int(&value, &value));
    }

    #[test]
    fn test_horner_exponentiation() {
        let p = hex_to_big_int("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFC2F")
            .unwrap();
        let expected = concat!(
            "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8FFFFE5490000000000000000",
            "000000000000000000000000000000150000A04A0131DD34FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF",
            "FFFFFFDCFFFE6F46FA06ADEF667F4E8D000000000000000000000000000000230002164C0BF2A430",
            "6602E2CCD42537E2FFFFFFFFFFFFFFFFFFFFFFEAFFFE6F46F40D5BC066FB804B834DEEA46B936D9B",
            "00000000000000070000A04A05F9522766031CCE7CF47B0E7D559AF77B739126FFFFFFFEFFFFE548",
            "FECE22C3667F318C2B985E6A1716F73F74B125318D22B84F"
        );

        assert_eq!(
            horner_exponentiation_big_int(&p, &u64_to_big_int(7)),
            hex_to_big_int(expected).unwrap()
        );
    }

    #[test]
    fn test_square_big_int_max_value() {
        let max_value = BigInt {
            limbs: [u64::MAX; BIG_INT_LIMBS],
        };

        assert_eq!(square_big_int(&max_value), u64_to_big_int(1));
    }
}
