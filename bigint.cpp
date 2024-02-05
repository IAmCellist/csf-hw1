#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iostream>
#include <iterator>
#include <sstream>
#include <stdexcept>
#include <string>
#include <sys/types.h>
#include <vector>
#include "bigint.h"

BigInt::BigInt() {
  magnitude = {0};
  sign = false;
}

BigInt::BigInt(uint64_t val, bool negative) {
  magnitude = {};
  magnitude.push_back(val);
  sign = negative;
}

BigInt::BigInt(std::initializer_list<uint64_t> vals, bool negative) {
  magnitude = {};
  for (auto element :  vals) {
    magnitude.push_back(element);
  }
  sign = negative ? negative : false;
}

BigInt::BigInt(const BigInt &other) {
 for (auto elem : other.get_bit_vector()) {
  magnitude.push_back(elem);
 }
 sign = other.sign;
}

BigInt::~BigInt() {
  
}

BigInt &BigInt::operator=(const BigInt &rhs) {
  magnitude = rhs.magnitude;
  sign = rhs.sign;
}

bool BigInt::is_negative() const {
  return sign;
}

bool BigInt::is_zero() const {
  for (int i = 0; i < magnitude.size(); i++) {
    if (get_bits(i) != 0) {
      return false;
    }
  }
  return true;
}

const std::vector<uint64_t> &BigInt::get_bit_vector() const {
  return magnitude;
}

uint64_t BigInt::get_bits(unsigned index) const {
  return index > magnitude.size() - 1 ? 0 : magnitude.at(index);
}

std::vector<uint64_t> BigInt::add_magnitudes(const BigInt &lhs, const BigInt &rhs) {
  std::vector<uint64_t> output = {};
  size_t length = std::max(lhs.get_bit_vector().size(), rhs.get_bit_vector().size());
  uint64_t carry = 0;

  for (size_t i = 0; i < length; i++) {
    uint64_t sum = lhs.get_bits(i) + rhs.get_bits(i) + carry;
    (sum < lhs.get_bits(i) || sum < rhs.get_bits(i)) ? carry = 1 : carry = 0;
    output.push_back(sum);
  }
  
  if (carry == 1) {
    output.push_back(carry);
  }

  return output;
}

std::vector<uint64_t> BigInt::subtract_magnitudes(const BigInt &lhs, const BigInt &rhs) {
  std::vector<uint64_t> output = {};
  size_t length = std::max(lhs.get_bit_vector().size(), rhs.get_bit_vector().size());
  uint64_t borrow = 0;

  for (size_t i = 0; i < length; i++) {
    uint64_t diff;
    uint64_t l_bit = lhs.get_bits(i) - borrow;
    uint64_t r_bit = rhs.get_bits(i);

    if (lhs.get_bits(i) < rhs.get_bits(i)) {
      borrow = 1;
      diff = (UINT64_MAX) - (r_bit - l_bit);
      diff = diff + 1;
    } else {
      borrow = 0;
      diff = l_bit - r_bit;
    }

    if (!(i + 1 == length && diff == 0)) {
       output.push_back(diff);
    }
  }

  return output;
}

int BigInt::compare_magnitudes(const BigInt &lhs, const BigInt &rhs) {
  size_t length = std::max(lhs.get_bit_vector().size(), rhs.get_bit_vector().size());

  if (length == 1) {
    if (lhs.get_bits(0) > rhs.get_bits(0)) return -1;
    else if (lhs.get_bits(0) < rhs.get_bits(0)) return 1;
    else return 0;
  }

  for (auto i = length - 1; i >= 0; i--) {
    if (lhs.get_bits(i) > rhs.get_bits(i)) {
      return -1;
    } else if (lhs.get_bits(i) < rhs.get_bits(i)) {
      return 1;
    }
  }

  return 0;
}

BigInt BigInt::simple_add(const BigInt &a, const BigInt &b) const {
  BigInt output = BigInt();
  output.sign = a.sign;
  output.magnitude = add_magnitudes(a, b);
  return output;
  
}

BigInt BigInt::mixed_add(const BigInt &a, const BigInt &b) const {
  BigInt output = BigInt();

  if (compare_magnitudes(a, b) == -1) {
    //a is larger than b
    output.magnitude = subtract_magnitudes(a, b);
    output.sign = a.sign;
  } else if (compare_magnitudes(a, b) == 1) {
    //b is larger than a
    output.magnitude = subtract_magnitudes(b, a);
    output.sign = b.sign;
  } else {
    //they have the same magnitude, but differing signs, which means it's 0
    output.magnitude.push_back(0);
    output.sign = false;
  }

  return output;
}

BigInt BigInt::operator+(const BigInt &rhs) const {
  if (this->sign == rhs.sign) {
    return simple_add(*this, rhs);
  }
  return mixed_add(*this, rhs);
}

BigInt BigInt::operator-(const BigInt &rhs) const {
  if (rhs.is_zero()) {
    return *this;
  }

  BigInt neg_rhs = rhs;
  neg_rhs.sign = !neg_rhs.sign;

  if (this->sign == neg_rhs.sign) {
    return simple_add(*this, neg_rhs);
  }
  return mixed_add(*this, neg_rhs);
}

BigInt BigInt::operator-() const {
  BigInt output = *this;
  output.sign = !sign;

  if (magnitude.size() == 1 && magnitude[0] == 0) {
    output.sign = false;
  }

  return output;
}

bool BigInt::is_bit_set(unsigned n) const {
  unsigned bit_index = n;
  int chunk = 0;

  while (bit_index / 64 != 0) {
    chunk++;
    bit_index = bit_index - 64;
  }

  //isolating what chunk we're working in
  uint64_t quotient = get_bits(chunk);
  uint64_t remainder = 0;

  for (int i = 0; i < bit_index + 1; i++) {
    remainder = quotient % 2;
    quotient = quotient / 2;
  }

  return remainder == 1;
}

BigInt BigInt::operator<<(unsigned n) const {
  BigInt output = *this;
  if (is_negative()) {
    throw std::invalid_argument("BigInt is negative");
  }
  output.sign = false;
  
  unsigned shift_words = n / 64;
  unsigned shift_bits = n % 64;

  uint64_t carry = 0;

  for (int i = 0; i < get_bit_vector().size(); i++) {
    uint64_t shifted_bit = output.magnitude[i] << shift_bits;
    output.magnitude[i] = shifted_bit | carry;

    carry = 0;

    if (shift_bits > 0) {
      carry = magnitude[i] >> (64 - shift_bits);
    }
  }

  if (carry != 0) {
    output.magnitude.push_back(carry);
  }

  if (shift_words > 0) {
    output.magnitude.insert(output.magnitude.begin(), shift_words, 0);
  }

  return output;
}

BigInt BigInt::operator*(const BigInt &rhs) const {
  BigInt result = BigInt();

  for (int i = 0; i < magnitude.size() * 64; i++) {
    if (is_bit_set(i)) {
      BigInt partial_product = rhs << i;
      result = result + partial_product;
    }
  }

  if (sign != rhs.sign) {
    result.sign = true;
  }
  
  return result;
}

BigInt BigInt::div_by_2() {
  BigInt output = *this;
  uint64_t carry = 0;

   for (int i = output.magnitude.size() - 1; i >= 0; --i) {
    uint64_t curr_chunk = output.magnitude[i];
    uint64_t lsb = curr_chunk & 1;

    uint64_t shifted_chunk = curr_chunk >> 1;
    output.magnitude[i] >>= 1;
    output.magnitude[i] |= (carry << 63);

    carry = lsb;
  }

  return output;
}

BigInt BigInt::operator/(const BigInt &rhs) const {
  BigInt output = BigInt();
  BigInt dividend = *this;
  dividend.sign = false;

  if (rhs.is_zero()) {
    throw std::invalid_argument("RHS cannot be zero.");
  }

  BigInt lower = BigInt();
  BigInt upper = dividend;
  BigInt mid = BigInt();

  while (lower < upper) {
    mid = (lower + upper).div_by_2();
    BigInt search_term = mid * rhs;
    if (search_term > dividend) {
      upper = mid - 1;
    } else if (search_term < dividend) {
      lower = mid + 1;
    }
  }

  bool test = lower < upper;
  output = lower == upper ? lower : upper;

  if ((is_negative() && !rhs.is_negative()) || (!is_negative() && rhs.is_negative())) {
    output.sign = true;
  } else {
    output.sign = false;
  }

  return output;
}

int BigInt::compare(const BigInt &rhs) const {
  if (is_zero() && rhs.is_zero()) {
    return 0;
  }

  if (!sign && rhs.sign) {
    //lhs is positive and rhs is negative
    return 1;
  } else if (sign && !rhs.sign) {
    //lhs is negative and rhs is positvive
    return -1;
  }

  int output = compare_magnitudes(*this, rhs) == 0 ? 0 : -compare_magnitudes(*this, rhs);

  return output;

}

std::string BigInt::to_hex() const {
  std::stringstream output;
  bool inserted_sig_bit = false;
  if (sign && !(magnitude.size() == 1 && magnitude[0] == 0)) {
    output << "-";
  }
  output << std::hex;

  std::vector<uint64_t>::const_reverse_iterator it = magnitude.rbegin();

  while (it != magnitude.rend()) {
    if (it == magnitude.rbegin()) {
      if (*it != 0 || magnitude.size() == 1) {
        output << *it;
        inserted_sig_bit = true;
        output << std::setfill('0') << std::setw(16);
      }
    } else if (inserted_sig_bit || *it != 0 || std::distance(it, magnitude.rend()) == 1){
      output << *it;
      inserted_sig_bit = true;
      output << std::setfill('0') << std::setw(16);
    }
    it++;
  }

  std::string test = output.str();
  return test;

}

std::string BigInt::to_dec() const {
  
}

