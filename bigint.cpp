#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <iomanip>
#include <iterator>
#include <sstream>
#include <string>
#include <sys/types.h>
#include <vector>
#include "bigint.h"

BigInt::BigInt() {
  magnitude = {0};
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

BigInt &BigInt::operator=(const BigInt &rhs)
{
  // TODO: implement
}

bool BigInt::is_negative() const {
  return sign;
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
    sum < lhs.get_bits(i) ? carry = 1 : carry = 0;
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
    output.push_back(diff);
  }

  return output;
}

int BigInt::compare_magnitudes(const BigInt &lhs, const BigInt &rhs) {
  size_t length = std::max(lhs.get_bit_vector().size(), rhs.get_bit_vector().size());

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

BigInt BigInt::operator-(const BigInt &rhs) const
{
  // TODO: implement
  // Hint: a - b could be computed as a + -b
}

BigInt BigInt::operator-() const {
  BigInt output = *this;
  output.sign = !sign;

  if (magnitude.size() == 1 && magnitude[0] == 0) {
    output.sign = false;
  }

  return output;
}

bool BigInt::is_bit_set(unsigned n) const
{
  // TODO: implement
}

BigInt BigInt::operator<<(unsigned n) const
{
  // TODO: implement
}

BigInt BigInt::operator*(const BigInt &rhs) const
{
  // TODO: implement
}

BigInt BigInt::operator/(const BigInt &rhs) const
{
  // TODO: implement
}

int BigInt::compare(const BigInt &rhs) const
{
  // TODO: implement
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

std::string BigInt::to_dec() const
{
  // TODO: implement
}

