#ifndef AOC_H
#define AOC_H

#include <vector>
#include <string>
#include <sstream>
#include <fstream>
#include <string_view>

std::vector<std::string> split_newline(const std::string& str);

std::vector<std::string> split_whitespace(const std::string& str);

std::string rdfile(const std::string& path);

std::string remove_prefix(const std::string& str, const std::string& prefix);

std::string remove_suffix(const std::string& str, const std::string& suffix);

bool has_prefix(const std::string& str, const std::string& prefix);

bool has_suffix(const std::string& str, const std::string& suffix);

std::vector<std::string> split(const std::string& s, const std::string& delimiter);

std::vector<int> extract_integers(const std::string& str);

std::vector<long long> extract_lls(const std::string& str);

std::vector<unsigned long> extract_unsigned_longs(const std::string& str);

std::string before_or_same(const std::string& s, const std::string& sep);

std::string after_or_same(const std::string& s, const std::string& sep);

#endif // AOC_H
