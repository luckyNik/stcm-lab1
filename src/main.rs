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

fn gcd_big_int(a: &BigInt, b: &BigInt) -> Option<BigInt> {
    let (a, b) = if matches!(compare_big_int(a, b), Comparison::Less) {
        (b, a)
    } else {
        (a, b)
    };

    let zero = u64_to_big_int(0);
    if a == &zero {
        return None;
    }
    if b == &zero {
        return Some(a.clone());
    }

    let mut current = b.clone();
    let mut next = divide_mod_big_int(a, b).unwrap().1;
    while next != zero {
        let previous = current;
        current = next;
        next = divide_mod_big_int(&previous, &current).unwrap().1;
    }
    Some(current)
}

fn lcm_big_int(a: &BigInt, b: &BigInt) -> Option<BigInt> {
    let zero = u64_to_big_int(0);
    if a == &zero || b == &zero {
        return None;
    }
    let gcd = gcd_big_int(a, b)?;
    let quotient = divide_mod_big_int(a, &gcd)?.0;
    Some(mult_big_int(&quotient, b))
}

fn kill_big_int_last_limbs(a: &mut BigInt, limb_count: usize) {
    for i in 0..BIG_INT_LIMBS {
        a.limbs[i] = if i + limb_count < BIG_INT_LIMBS {
            a.limbs[i + limb_count]
        } else {
            0
        };
    }
}

struct BarrettModulus {
    n: BigInt,
    mu: BigInt,
    k: usize,
}

fn barrett_precompute_big_int(n: &BigInt) -> Option<BarrettModulus> {
    let k = bit_length_big_int(n).div_ceil(64);
    if k == 0 || 2 * k + 2 > BIG_INT_LIMBS {
        return None;
    }

    let mut beta_power = u64_to_big_int(1);
    shift_big_int_significant_limbs(&mut beta_power, 2 * k);
    let mu = divide_mod_big_int(&beta_power, n)?.0;

    Some(BarrettModulus {
        n: n.clone(),
        mu,
        k,
    })
}

fn barrett_reduction_big_int(x: &BigInt, modulus: &BarrettModulus) -> Option<BigInt> {
    if matches!(compare_big_int(x, &modulus.n), Comparison::Less) {
        return Some(x.clone());
    }

    let k = modulus.k;
    if bit_length_big_int(x).div_ceil(64) > 2 * k {
        return None;
    }

    let mut q = x.clone();
    kill_big_int_last_limbs(&mut q, k - 1);
    q = mult_big_int(&q, &modulus.mu);
    kill_big_int_last_limbs(&mut q, k + 1);

    let mut r = subtract_big_int(x, &mult_big_int(&q, &modulus.n)).unwrap();
    while !matches!(compare_big_int(&r, &modulus.n), Comparison::Less) {
        r = subtract_big_int(&r, &modulus.n).unwrap();
    }
    Some(r)
}

fn add_mod_big_int(a: &BigInt, b: &BigInt, modulus: &BarrettModulus) -> Option<BigInt> {
    let a = barrett_reduction_big_int(a, modulus)?;
    let b = barrett_reduction_big_int(b, modulus)?;
    let sum = add_big_int(&a, &b);
    if matches!(compare_big_int(&sum, &modulus.n), Comparison::Less) {
        return Some(sum);
    }
    subtract_big_int(&sum, &modulus.n)
}

fn subtract_mod_big_int(a: &BigInt, b: &BigInt, modulus: &BarrettModulus) -> Option<BigInt> {
    let mut a = barrett_reduction_big_int(a, modulus)?;
    let b = barrett_reduction_big_int(b, modulus)?;
    if matches!(compare_big_int(&a, &b), Comparison::Less) {
        a = add_big_int(&a, &modulus.n);
    }
    subtract_big_int(&a, &b)
}

fn mult_mod_big_int(a: &BigInt, b: &BigInt, modulus: &BarrettModulus) -> Option<BigInt> {
    let a = barrett_reduction_big_int(a, modulus)?;
    let b = barrett_reduction_big_int(b, modulus)?;
    barrett_reduction_big_int(&mult_big_int(&a, &b), modulus)
}

fn square_mod_big_int(a: &BigInt, modulus: &BarrettModulus) -> Option<BigInt> {
    let a = barrett_reduction_big_int(a, modulus)?;
    barrett_reduction_big_int(&square_big_int(&a), modulus)
}

fn horner_exponentiation_mod_big_int(
    a: &BigInt,
    b: &BigInt,
    modulus: &BarrettModulus,
) -> Option<BigInt> {
    let a = barrett_reduction_big_int(a, modulus)?;
    let mut result = u64_to_big_int(1);

    let iterations = bit_length_big_int(b);
    for i in (0..iterations).rev() {
        result = barrett_reduction_big_int(&square_big_int(&result), modulus)?;
        if b.limbs[i / 64] & (1u64 << (i % 64)) != 0 {
            result = barrett_reduction_big_int(&mult_big_int(&result, &a), modulus)?;
        }
    }
    Some(result)
}

fn window_exponentiation_mod_big_int(
    a: &BigInt,
    b: &BigInt,
    modulus: &BarrettModulus,
) -> Option<BigInt> {
    let a = barrett_reduction_big_int(a, modulus)?;

    let mut powers = Vec::with_capacity(16);
    powers.push(u64_to_big_int(1));
    for i in 1..16 {
        powers.push(barrett_reduction_big_int(
            &mult_big_int(&powers[i - 1], &a),
            modulus,
        )?);
    }

    let mut result = u64_to_big_int(1);
    let windows = bit_length_big_int(b).div_ceil(4);
    for i in (0..windows).rev() {
        for _ in 0..4 {
            result = barrett_reduction_big_int(&square_big_int(&result), modulus)?;
        }
        let window = (b.limbs[i * 4 / 64] >> (i * 4 % 64)) & 0xF;
        if window != 0 {
            result = barrett_reduction_big_int(
                &mult_big_int(&result, &powers[window as usize]),
                modulus,
            )?;
        }
    }
    Some(result)
}

struct BenchmarkInput {
    a: BigInt,
    b: BigInt,
    exponent: BigInt,
}

struct ModBenchmarkInput {
    a: BigInt,
    b: BigInt,
    product: BigInt,
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

    let modulus_limbs = BIG_INT_LIMBS / 2 - 1;
    let mut n = BigInt {
        limbs: std::array::from_fn(|i| if i < modulus_limbs { rng.random() } else { 0 }),
    };
    n.limbs[modulus_limbs - 1] |= 1u64 << 63;
    let modulus = barrett_precompute_big_int(&n).unwrap();

    let mut mod_inputs = Vec::with_capacity(10_000);
    for _ in 0..10_000 {
        let a = BigInt {
            limbs: std::array::from_fn(|i| if i < modulus_limbs { rng.random() } else { 0 }),
        };
        let b = BigInt {
            limbs: std::array::from_fn(|i| if i < modulus_limbs { rng.random() } else { 0 }),
        };
        let a = barrett_reduction_big_int(&a, &modulus).unwrap();
        let b = barrett_reduction_big_int(&b, &modulus).unwrap();
        let product = mult_big_int(&a, &b);
        let exponent = BigInt {
            limbs: std::array::from_fn(|i| if i < modulus_limbs { rng.random() } else { 0 }),
        };
        mod_inputs.push(ModBenchmarkInput {
            a,
            b,
            product,
            exponent,
        });
    }

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(barrett_reduction_big_int(&input.product, &modulus));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(barrett_reduction_big_int(&input.product, &modulus));
    }
    let barrett_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Barrett reduction\n{barrett_average:.1} ns/op\n");

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(divide_mod_big_int(&input.product, &n));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(divide_mod_big_int(&input.product, &n));
    }
    let division_reduction_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Reduction by division\n{division_reduction_average:.1} ns/op\n");

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(add_mod_big_int(&input.a, &input.b, &modulus));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(add_mod_big_int(&input.a, &input.b, &modulus));
    }
    let add_mod_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Modular addition\n{add_mod_average:.1} ns/op\n");

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(subtract_mod_big_int(&input.a, &input.b, &modulus));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(subtract_mod_big_int(&input.a, &input.b, &modulus));
    }
    let subtract_mod_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Modular subtraction\n{subtract_mod_average:.1} ns/op\n");

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(mult_mod_big_int(&input.a, &input.b, &modulus));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(mult_mod_big_int(&input.a, &input.b, &modulus));
    }
    let mult_mod_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Modular multiplication\n{mult_mod_average:.1} ns/op\n");

    for input in &mod_inputs[..1_000] {
        std::hint::black_box(square_mod_big_int(&input.a, &modulus));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs {
        std::hint::black_box(square_mod_big_int(&input.a, &modulus));
    }
    let square_mod_average = started.elapsed().as_nanos() as f64 / 10_000.0;
    println!("Modular square\n{square_mod_average:.1} ns/op\n");

    for input in &mod_inputs[..100] {
        std::hint::black_box(gcd_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs[..1_000] {
        std::hint::black_box(gcd_big_int(&input.a, &input.b));
    }
    let gcd_average = started.elapsed().as_nanos() as f64 / 1_000.0;
    println!("GCD\n{gcd_average:.1} ns/op\n");

    for input in &mod_inputs[..100] {
        std::hint::black_box(lcm_big_int(&input.a, &input.b));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs[..1_000] {
        std::hint::black_box(lcm_big_int(&input.a, &input.b));
    }
    let lcm_average = started.elapsed().as_nanos() as f64 / 1_000.0;
    println!("LCM\n{lcm_average:.1} ns/op\n");

    for input in &mod_inputs[..10] {
        std::hint::black_box(horner_exponentiation_mod_big_int(
            &input.a,
            &input.exponent,
            &modulus,
        ));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs[..100] {
        std::hint::black_box(horner_exponentiation_mod_big_int(
            &input.a,
            &input.exponent,
            &modulus,
        ));
    }
    let horner_mod_average = started.elapsed().as_nanos() as f64 / 100.0;
    println!("Horner modular exponentiation\n{horner_mod_average:.1} ns/op\n");

    for input in &mod_inputs[..10] {
        std::hint::black_box(window_exponentiation_mod_big_int(
            &input.a,
            &input.exponent,
            &modulus,
        ));
    }
    let started = std::time::Instant::now();
    for input in &mod_inputs[..100] {
        std::hint::black_box(window_exponentiation_mod_big_int(
            &input.a,
            &input.exponent,
            &modulus,
        ));
    }
    let window_mod_average = started.elapsed().as_nanos() as f64 / 100.0;
    println!("Window (4 bits) modular exponentiation\n{window_mod_average:.1} ns/op\n");
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

    #[test]
    fn test_add_big_int_1024() {
        assert_eq!(add_big_int(&hex_to_big_int("1cfe4177fcb132f70cea7cd2b8fc2ab59995f89f6b84e3802e01883cd2aabac09263f8c6c86dff224b40c7f8dea2002edbccae774c33c947ef9761fd446cca6b24df6d2cdf45936f411c089d3039d86d028247ba8586960bdb4eea57d648576564883c85c735d1290c07872f9e1ff57688009b83071c2d3cd5918acf86fc0e84").unwrap(), &hex_to_big_int("202171731bd25f23cb5723b759bce0c00632334c4e9917fc14be434bf9db4b8653c39df5c563ea9c5e314de587a68261a407ad8512ba79993b280022e269ed74f37bfe8e74bafb86682da9034d76c812556e60745617ca65ab62ea46746ef58b96c852f237bf18c53e30f21eacdc84fd42ebaad5c247490cb7d55e879a12707c").unwrap()), hex_to_big_int("3d1fb2eb1883921ad841a08a12b90b759fc82bebba1dfb7c42bfcb88cc860646e62796bc8dd1e9bea97215de664882907fd45bfc5eee42e12abf622026d6b7e0185b6bbb54008ef5a949b1a07db0a07f57f0a82edb9e607186b1d49e4ab74cf0fb508f77fef4e9ee4a38794e4afc7a73caec4658c96376498d66e957210e7f00").unwrap());
    }

    #[test]
    fn test_subtract_big_int_1024() {
        assert_eq!(subtract_big_int(&hex_to_big_int("202171731bd25f23cb5723b759bce0c00632334c4e9917fc14be434bf9db4b8653c39df5c563ea9c5e314de587a68261a407ad8512ba79993b280022e269ed74f37bfe8e74bafb86682da9034d76c812556e60745617ca65ab62ea46746ef58b96c852f237bf18c53e30f21eacdc84fd42ebaad5c247490cb7d55e879a12707c").unwrap(), &hex_to_big_int("1cfe4177fcb132f70cea7cd2b8fc2ab59995f89f6b84e3802e01883cd2aabac09263f8c6c86dff224b40c7f8dea2002edbccae774c33c947ef9761fd446cca6b24df6d2cdf45936f411c089d3039d86d028247ba8586960bdb4eea57d648576564883c85c735d1290c07872f9e1ff57688009b83071c2d3cd5918acf86fc0e84").unwrap()).unwrap(), hex_to_big_int("3232ffb1f212c2cbe6ca6e4a0c0b60a6c9c3aace314347be6bcbb0f273090c5c15fa52efcf5eb7a12f085eca9048232c83aff0dc686b0514b909e259dfd2309ce9c9161957568172711a0661d3cefa552ec18b9d0913459d013ffee9e269e263240166c7089479c32296aef0ebc8f86baeb0f52bb2b1bcfe243d3b8131661f8").unwrap());
    }

    #[test]
    fn test_mult_big_int_1024() {
        assert_eq!(mult_big_int(&hex_to_big_int("1cfe4177fcb132f70cea7cd2b8fc2ab59995f89f6b84e3802e01883cd2aabac09263f8c6c86dff224b40c7f8dea2002edbccae774c33c947ef9761fd446cca6b24df6d2cdf45936f411c089d3039d86d028247ba8586960bdb4eea57d648576564883c85c735d1290c07872f9e1ff57688009b83071c2d3cd5918acf86fc0e84").unwrap(), &hex_to_big_int("202171731bd25f23cb5723b759bce0c00632334c4e9917fc14be434bf9db4b8653c39df5c563ea9c5e314de587a68261a407ad8512ba79993b280022e269ed74f37bfe8e74bafb86682da9034d76c812556e60745617ca65ab62ea46746ef58b96c852f237bf18c53e30f21eacdc84fd42ebaad5c247490cb7d55e879a12707c").unwrap()), hex_to_big_int("3a391ceb451bce6f2b8dbc9d3e6ce221cf7f195cfc77ac5e18d46031fc321fadd53fddfe4d20f503128a592b0cd5f9ae28338464bca058f9a957db2ef283b0f7789b8b134eff86bafc2e25ccebe4b8460933f78230a2253469c574ce665b143d8fa1d5c73a17560d22a3c0f7a19e662bbedbc52b778d70faebf8ee3414084fda40c2b604687af338d48e7da8453e9244d4a253f7e97d6618064d94ba2ec26984bc8ed5fe20aa88ebfd56c59aeda8207bd2d0e17329549838e6da9688f518a328edc9e01626463628cb6d6fc4f835ff1d2824b9b07dde52ca0c4a69c94886c8d6d4d75507be93fb5c808d3e4f4157630725be58cb7a34fece51945a415b8c7f0").unwrap());
    }

    #[test]
    fn test_div_mod_big_int_1024() {
        assert_eq!(divide_mod_big_int(&hex_to_big_int("202171731bd25f23cb5723b759bce0c00632334c4e9917fc14be434bf9db4b8653c39df5c563ea9c5e314de587a68261a407ad8512ba79993b280022e269ed74f37bfe8e74bafb86682da9034d76c812556e60745617ca65ab62ea46746ef58b96c852f237bf18c53e30f21eacdc84fd42ebaad5c247490cb7d55e879a12707c").unwrap(), &hex_to_big_int("b406f842b945341d32d8eceaddedd3046cf46347d09fca1f528785532eb8f16e5338791703dbc418dfa130c310bf6225627231bcd78ce64b3d4dcac838f16318").unwrap()).unwrap(), (hex_to_big_int("2db0a38e93191599656dab9f24977f3bfa002f3ccd724e730d2cba049b78eddf331c2994c0c33f2a0f8366e6a04388984d5b8fedb5c6846a10b11a333e576c45").unwrap(), hex_to_big_int("15a0f36e30c88ca860f76e4fb1c3d260cd466638ceb9593eb2aa9e774e47b172409accb6ab98889e69880fabe605adddd638636c3f0933a918bc40b9ee0c9b04").unwrap()));
    }

    #[test]
    fn test_gcd_zero() {
        let zero = u64_to_big_int(0);
        let x = u64_to_big_int(42);

        assert_eq!(gcd_big_int(&x, &zero), Some(x.clone()));
        assert_eq!(gcd_big_int(&zero, &x), Some(x));
        assert_eq!(gcd_big_int(&zero, &zero), None);
    }

    #[test]
    fn test_gcd_a_greater_or_equal_b() {
        let a = hex_to_big_int("22f806111e4175727f0c676d823c8c4708c8bcde23205e33b19e4c3fd2fa1e200a0e200026e5a0b88ce3f61799dba2c6ce653d9ed97ba2bf5ca1600c5656f5839fcc2f7385b29aee20fe96c11fa51cdcc1ffd601db0c71").unwrap();
        let b = hex_to_big_int("16d6034ab6e42209ce13868ce50fc765eb94d6c83b480b11a41ae5042ab9bad295c89b95942201bc6e06d5b1c6b2f30e07b0f129cfce999d26383f716a898576efb18f70e5c69bd055d8693cd4db6ee44986c53817").unwrap();
        let expected = hex_to_big_int(
            "181892f902bd23f0824128b2f330c5c7fd0a6a3a4506513270e269e0d37f2a74de452e6b439",
        )
        .unwrap();

        assert_eq!(gcd_big_int(&a, &b), Some(expected));
        assert_eq!(gcd_big_int(&a, &a), Some(a));
    }

    #[test]
    fn test_gcd_a_less_than_b() {
        let a = hex_to_big_int("16d6034ab6e42209ce13868ce50fc765eb94d6c83b480b11a41ae5042ab9bad295c89b95942201bc6e06d5b1c6b2f30e07b0f129cfce999d26383f716a898576efb18f70e5c69bd055d8693cd4db6ee44986c53817").unwrap();
        let b = hex_to_big_int("22f806111e4175727f0c676d823c8c4708c8bcde23205e33b19e4c3fd2fa1e200a0e200026e5a0b88ce3f61799dba2c6ce653d9ed97ba2bf5ca1600c5656f5839fcc2f7385b29aee20fe96c11fa51cdcc1ffd601db0c71").unwrap();
        let expected = hex_to_big_int(
            "181892f902bd23f0824128b2f330c5c7fd0a6a3a4506513270e269e0d37f2a74de452e6b439",
        )
        .unwrap();

        assert_eq!(gcd_big_int(&a, &b), Some(expected));
    }

    #[test]
    fn test_lcm_big_int() {
        let a = hex_to_big_int("22f806111e4175727f0c676d823c8c4708c8bcde23205e33b19e4c3fd2fa1e200a0e200026e5a0b88ce3f61799dba2c6ce653d9ed97ba2bf5ca1600c5656f5839fcc2f7385b29aee20fe96c11fa51cdcc1ffd601db0c71").unwrap();
        let b = hex_to_big_int("16d6034ab6e42209ce13868ce50fc765eb94d6c83b480b11a41ae5042ab9bad295c89b95942201bc6e06d5b1c6b2f30e07b0f129cfce999d26383f716a898576efb18f70e5c69bd055d8693cd4db6ee44986c53817").unwrap();
        let expected = hex_to_big_int("2123e9b5a09c20eafe8fbeb494bf8ac9ab36a24da2452445cc1b1470d27f7caa5e385a38f49989a64d80637a23e087b6630a27dc8f01a437cc2f1c2a4ae22e0f1f8b392c9ed19db09266b76982d05851a04b730925a288cf780d83644ffacbc172045e0fbd6e09f40e66b7b5cfecd260a92b2e3133c8393d84f03483b51e765811b03aff89d5f").unwrap();

        assert_eq!(lcm_big_int(&a, &b), Some(expected.clone()));
        assert_eq!(lcm_big_int(&b, &a), Some(expected));
    }

    #[test]
    fn test_lcm_zero() {
        let zero = u64_to_big_int(0);
        let x = u64_to_big_int(42);

        assert_eq!(lcm_big_int(&x, &zero), None);
        assert_eq!(lcm_big_int(&zero, &x), None);
        assert_eq!(lcm_big_int(&zero, &zero), None);
    }

    const MOD_N: &str = "2029873D6C51E370EB9A0A96263AE6C5E818FAC0433CBD7DABE929C4A334BFC6CD75E9BB049A79D7A7A3CC8C3D5F169293DE8FC88B28756BAD6BE28E7AA6E99F19950499DD251DE512148239292D22E255ACCB1A466884F3F49249DC28FF90A5AEC7978306D03BF38B2FFC80A4DF5A51C9BC701E7EA419";
    const MOD_A: &str = "1723BB1389B372A341738C837A7935BEF7E268FFE976AB60581CCACE1D62E05B4C8012EDE7BD0CFFB88309FADB890859001AC9406329BC65B00A2D35D148805071950EADEC6F117D836E77AF67D461E4163207D094499602F0EE99731C94521919E93AD11745AD498893101C593AF514AA4E719D3C7DEC";
    const MOD_B: &str = "6CE1DCF884CDE0279E17F9AC0988DF05F2595F19A51E41686CD66160227173714726C1672297608D9425D111A9D5E6C9992B5FB12E0D9090B89065550964F1A8A1D93D20470B766FF10B437BDB5A51149BBE060A72424114258751B4C8349A047DC4AC87FC089BE9C1C8EB5140F16F4488157241955";

    #[test]
    fn test_kill_big_int_last_limbs() {
        let mut a = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        a.limbs[0] = 5;
        a.limbs[1] = 7;
        a.limbs[2] = 9;

        kill_big_int_last_limbs(&mut a, 1);

        let mut expected = BigInt {
            limbs: [0; BIG_INT_LIMBS],
        };
        expected.limbs[0] = 7;
        expected.limbs[1] = 9;
        assert_eq!(a, expected);
    }

    #[test]
    fn test_barrett_precompute_invalid_modulus() {
        let mut too_long = u64_to_big_int(1);
        shift_big_int_significant_limbs(&mut too_long, BIG_INT_LIMBS / 2 - 1);

        assert!(barrett_precompute_big_int(&u64_to_big_int(0)).is_none());
        assert!(barrett_precompute_big_int(&too_long).is_none());
    }

    #[test]
    fn test_barrett_reduction() {
        let n = hex_to_big_int(MOD_N).unwrap();
        let modulus = barrett_precompute_big_int(&n).unwrap();
        let x = hex_to_big_int("EBE5C24C1AE7D510557ED4D19B885DC0A68CDB54088754E64F4F5EE8C7298E6E5B7E1CD7BE8A7D83351265C28EA0879D955025FF87C44DF8A13D4F398EE4B5C1A9533D918086EDD77D866E61127E26B524ACE0D8D877A98B9ACB2C55523807C7E30A598D0DBCCBFD2EC8C53765F4EC0A954FF8B2A6AAB74FE5766EEBC578F4ECB4F4041F5EE8BAE8E66FA97002CFCBAD167F5A9CA5FEDF165DAB6EAFFF5782AFE6BAC9F21DF74F09AF5B3618E1CA06D7A691F3C42B2E2CBBB93D98145593A9AFA39E261E34A7B6BC3C62475263C785490146DEDC86A9F4FB02BB7A1774F1A42721EABA4C70EE306F0C485F184E0").unwrap();
        let expected = hex_to_big_int("1DA7A784A21A06F46E8D931A4BB2B6714EF065DAFEEF4CC3FE0D238DAC600966173C60876FA38F15B8491838BC9156B251E711FA51BFC5874C54E55862C17C284EA556C63453B3951366143A22591220AD852442D5FC4BEF04FB9C14D5E439EA61B48E2E9D8BC3ECADB3C3D3A7C152446D76B1C4AEBA97").unwrap();

        assert_eq!(barrett_reduction_big_int(&x, &modulus), Some(expected));
    }

    #[test]
    fn test_barrett_reduction_division_consistency() {
        let n = hex_to_big_int(MOD_N).unwrap();
        let modulus = barrett_precompute_big_int(&n).unwrap();
        let x = mult_big_int(
            &hex_to_big_int(MOD_A).unwrap(),
            &hex_to_big_int(MOD_B).unwrap(),
        );

        assert_eq!(
            barrett_reduction_big_int(&x, &modulus),
            Some(divide_mod_big_int(&x, &n).unwrap().1)
        );
    }

    #[test]
    fn test_barrett_reduction_too_long() {
        let n = hex_to_big_int(MOD_N).unwrap();
        let modulus = barrett_precompute_big_int(&n).unwrap();
        let mut x = u64_to_big_int(1);
        shift_big_int_significant_limbs(&mut x, 2 * modulus.k);

        assert_eq!(barrett_reduction_big_int(&x, &modulus), None);
    }

    #[test]
    fn test_add_mod_big_int() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let b = hex_to_big_int(MOD_B).unwrap();
        let expected = hex_to_big_int("172A8931593BBF8143ED6E031539CE4CE8418E95DB10FD446EA398343365077283948559FE2F3675C15C4C57ECA3A5B76CB45BF65E3C9D3EB915B63C2699169F8C1F2C41BE738234EA6D88639F921789277BC3B0F4F0BA270230F1E837E0D562BA31171BDFC56DD3472F2CAB0E4F042B9E96F2F4609741").unwrap();

        assert_eq!(add_mod_big_int(&a, &b, &modulus), Some(expected));
    }

    #[test]
    fn test_add_mod_big_int_wrap() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let expected = hex_to_big_int("E1DEEE9A71501D5974D0E70CEB784B807ABD73F8FB0994304506BD7979100EFCB8A3C20CADFA027C962476979B2FA1F6C5702B83B2B035FB2A877DD27EA1701C99518C1FBB90515F4C86D25A67BA0E5D6B74486E22AA711ED4AE90A1029138C850ADE1F27BB1E9F85F623B80D968FD78AE0731BFA57BF").unwrap();

        assert_eq!(add_mod_big_int(&a, &a, &modulus), Some(expected));
    }

    #[test]
    fn test_subtract_mod_big_int_a_greater_than_b() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let b = hex_to_big_int(MOD_B).unwrap();
        let expected = hex_to_big_int("171CECF5BA2B25C53EF9AB03DFB89D3107834369F7DC597C4195FD680760B944156BA081D14AE389AFA9C79DCA6E6AFA9381368A6816DB8CA6FEA42F7BF7EA01570AF11A1A6AA0C61C6F66FB3016AC3F04E84BF033A271DEDFAC40FE0147CECF79A15E864EC5ECBFC9F6F38DA426E5FDB605F046186497").unwrap();

        assert_eq!(subtract_mod_big_int(&a, &b, &modulus), Some(expected));
    }

    #[test]
    fn test_subtract_mod_big_int_a_less_than_b() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let b = hex_to_big_int(MOD_B).unwrap();
        let expected = hex_to_big_int("90C9A47B226BDABACA05F9246824994E095B7564B6064016A532C5C9BD40682B80A4939334F964DF7FA04EE72F0AB98005D593E231199DF066D3E5EFEAEFF9DC28A137FC2BA7D1EF5A51B3DF91676A350C47F2A12C6131514E608DE27B7C1D6352638FCB80A4F33C13908F300B8745413B67FD8663F82").unwrap();

        assert_eq!(subtract_mod_big_int(&b, &a, &modulus), Some(expected));
    }

    #[test]
    fn test_mult_mod_big_int() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let b = hex_to_big_int(MOD_B).unwrap();
        let expected = hex_to_big_int("DCC05080E82169C2AB181192CD583A3E5B5E773751CE89EAD24D2C1047633C9A2CD88F4893DC59E0311B855D1B9B74BE4605594FF47507EF422E83B200FA0453D6038DF5CB16FADCA60CE073959519E8D74679C516416B41A873B1ABC70B338914404CAF8D89C0DB8A7F40ED4B6A76F7CE33A7AB01842").unwrap();

        assert_eq!(mult_mod_big_int(&a, &b, &modulus), Some(expected));
    }

    #[test]
    fn test_square_mod_big_int() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let expected = hex_to_big_int("1E5CD82B27D0462C63E40FF7C54398F3DE06B9A3F140A230DB9A5C7D42D6471AFD0144595510F7E3B033D6C9C29E0DFD003853C4CE17C3650E1D1045BD30CAF674C1FD9CA7322A5903099F9CD7DE84BD914758EE85423BD56CB8CD027880E46F647E3FB3224E20F31F77530601ED0D098C5CA2FF89589D").unwrap();

        assert_eq!(square_mod_big_int(&a, &modulus), Some(expected));
    }

    #[test]
    fn test_square_mult_mod_consistency() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();

        assert_eq!(
            square_mod_big_int(&a, &modulus),
            mult_mod_big_int(&a, &a, &modulus)
        );
    }

    #[test]
    fn test_horner_exponentiation_mod_big_int() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let d = hex_to_big_int("BF675299B0C83E786D1711CBD2106119EC40D31B5397A762393550840067F0CFCCE1FD3D9849ACFB58350A73F7AAC319FFB759E0FED1D9D1690624FE36B82E6C9D82FB0F1423674A6864FA3F3EAB06E9B65ED0DE47DB4304DE01C683E99A46DF0DDE3A361C0099EBACD73DE0081A0BA05").unwrap();
        let expected = hex_to_big_int("1A375B76AC747B2F14A5856454225F881E29201ACA43CA6BF386CFCA5472B06FF040826A8AFB683ACB9FFCE97846CA0F8CFA9D3D3506514B7573AE72B2736FDC7EA2A13C91C5EEAD7BD04D795A34D496C020BEEC4A40ADA7EBD350703EE2EF5E2F1E5138546C2459534BBD996923F57B3A5BE2147BF93F").unwrap();

        assert_eq!(
            horner_exponentiation_mod_big_int(&a, &d, &modulus),
            Some(expected)
        );
    }

    #[test]
    fn test_horner_exponentiation_mod_big_int_zero_exponent() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();

        assert_eq!(
            horner_exponentiation_mod_big_int(&a, &u64_to_big_int(0), &modulus),
            Some(u64_to_big_int(1))
        );
    }

    #[test]
    fn test_window_exponentiation_mod_big_int() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();
        let d = hex_to_big_int("BF675299B0C83E786D1711CBD2106119EC40D31B5397A762393550840067F0CFCCE1FD3D9849ACFB58350A73F7AAC319FFB759E0FED1D9D1690624FE36B82E6C9D82FB0F1423674A6864FA3F3EAB06E9B65ED0DE47DB4304DE01C683E99A46DF0DDE3A361C0099EBACD73DE0081A0BA05").unwrap();
        let expected = hex_to_big_int("1A375B76AC747B2F14A5856454225F881E29201ACA43CA6BF386CFCA5472B06FF040826A8AFB683ACB9FFCE97846CA0F8CFA9D3D3506514B7573AE72B2736FDC7EA2A13C91C5EEAD7BD04D795A34D496C020BEEC4A40ADA7EBD350703EE2EF5E2F1E5138546C2459534BBD996923F57B3A5BE2147BF93F").unwrap();

        assert_eq!(
            window_exponentiation_mod_big_int(&a, &d, &modulus),
            Some(expected)
        );
    }

    #[test]
    fn test_window_exponentiation_mod_big_int_zero_exponent() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();

        assert_eq!(
            window_exponentiation_mod_big_int(&a, &u64_to_big_int(0), &modulus),
            Some(u64_to_big_int(1))
        );
    }

    #[test]
    fn test_window_horner_exponentiation_mod_consistency() {
        let modulus = barrett_precompute_big_int(&hex_to_big_int(MOD_N).unwrap()).unwrap();
        let a = hex_to_big_int(MOD_A).unwrap();

        for exponent in 0..300 {
            let exponent = u64_to_big_int(exponent);
            assert_eq!(
                window_exponentiation_mod_big_int(&a, &exponent, &modulus),
                horner_exponentiation_mod_big_int(&a, &exponent, &modulus)
            );
        }
    }

    fn random_big_int(limbs: usize) -> BigInt {
        let mut rng = rand::rng();
        BigInt {
            limbs: std::array::from_fn(|i| if i < limbs { rng.random() } else { 0 }),
        }
    }

    fn random_modulus(limbs: usize) -> BarrettModulus {
        let mut n = random_big_int(limbs);
        n.limbs[limbs - 1] |= 1u64 << 63;
        barrett_precompute_big_int(&n).unwrap()
    }

    const PRIME_521: &str = "1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF";

    #[test]
    fn test_mod_distributivity() {
        for limbs in 1..BIG_INT_LIMBS / 2 {
            let modulus = random_modulus(limbs);
            let a = random_big_int(limbs);
            let b = random_big_int(limbs);
            let c = random_big_int(limbs);

            let sum = add_mod_big_int(&a, &b, &modulus).unwrap();
            let sum_times_c = mult_mod_big_int(&sum, &c, &modulus);
            let c_times_sum = mult_mod_big_int(&c, &sum, &modulus);
            let a_c_plus_b_c = add_mod_big_int(
                &mult_mod_big_int(&a, &c, &modulus).unwrap(),
                &mult_mod_big_int(&b, &c, &modulus).unwrap(),
                &modulus,
            );

            assert_eq!(sum_times_c, c_times_sum);
            assert_eq!(sum_times_c, a_c_plus_b_c);
        }
    }

    #[test]
    fn test_mod_mult_by_repeated_addition() {
        for limbs in 1..BIG_INT_LIMBS / 2 {
            let modulus = random_modulus(limbs);
            let a = random_big_int(limbs);
            let count = rand::rng().random_range(100..=200);

            let mut sum = u64_to_big_int(0);
            for _ in 0..count {
                sum = add_mod_big_int(&sum, &a, &modulus).unwrap();
            }

            assert_eq!(
                mult_mod_big_int(&u64_to_big_int(count), &a, &modulus),
                Some(sum)
            );
        }
    }

    #[test]
    fn test_euler_theorem_prime() {
        let n = hex_to_big_int(PRIME_521).unwrap();
        let modulus = barrett_precompute_big_int(&n).unwrap();
        let phi = subtract_big_int(&n, &u64_to_big_int(1)).unwrap();

        for _ in 0..3 {
            let a = random_big_int(modulus.k);

            assert_eq!(gcd_big_int(&a, &n), Some(u64_to_big_int(1)));
            assert_eq!(
                horner_exponentiation_mod_big_int(&a, &phi, &modulus),
                Some(u64_to_big_int(1))
            );
            assert_eq!(
                window_exponentiation_mod_big_int(&a, &phi, &modulus),
                Some(u64_to_big_int(1))
            );
        }
    }

    #[test]
    fn test_euler_theorem_power_of_three() {
        let three = u64_to_big_int(3);
        for k in [1, 40, 200, 605] {
            let n = horner_exponentiation_big_int(&three, &u64_to_big_int(k));
            let previous_power = horner_exponentiation_big_int(&three, &u64_to_big_int(k - 1));
            let phi = subtract_big_int(&n, &previous_power).unwrap();
            let modulus = barrett_precompute_big_int(&n).unwrap();

            for _ in 0..3 {
                let mut a = random_big_int(modulus.k);
                if divide_mod_big_int(&a, &three).unwrap().1 == u64_to_big_int(0) {
                    a = add_big_int(&a, &u64_to_big_int(1));
                }

                assert_eq!(gcd_big_int(&a, &n), Some(u64_to_big_int(1)));
                assert_eq!(
                    horner_exponentiation_mod_big_int(&a, &phi, &modulus),
                    Some(u64_to_big_int(1))
                );
                assert_eq!(
                    window_exponentiation_mod_big_int(&a, &phi, &modulus),
                    Some(u64_to_big_int(1))
                );
            }
        }
    }

    #[test]
    fn test_add_subtract_mod_inverse() {
        for limbs in 1..BIG_INT_LIMBS / 2 {
            let modulus = random_modulus(limbs);
            let a = random_big_int(limbs);
            let b = random_big_int(limbs);

            let sum = add_mod_big_int(&a, &b, &modulus).unwrap();

            assert_eq!(
                subtract_mod_big_int(&sum, &b, &modulus),
                barrett_reduction_big_int(&a, &modulus)
            );
        }
    }

    #[test]
    fn test_barrett_reduction_random_division_consistency() {
        for limbs in 1..BIG_INT_LIMBS / 2 {
            let modulus = random_modulus(limbs);
            let x = random_big_int(2 * limbs);

            assert_eq!(
                barrett_reduction_big_int(&x, &modulus),
                Some(divide_mod_big_int(&x, &modulus.n).unwrap().1)
            );
        }
    }

    #[test]
    fn test_gcd_lcm_product() {
        for _ in 0..5 {
            let g = random_big_int(5);
            let a = mult_big_int(&g, &random_big_int(5));
            let b = mult_big_int(&g, &random_big_int(5));
            let gcd = gcd_big_int(&a, &b).unwrap();

            assert_eq!(divide_mod_big_int(&gcd, &g).unwrap().1, u64_to_big_int(0));
            assert_eq!(
                mult_big_int(&gcd, &lcm_big_int(&a, &b).unwrap()),
                mult_big_int(&a, &b)
            );
        }
    }
}
