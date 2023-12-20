#include <fstream>
#include <sstream>
#include <string>
#include <vector>

std::vector<std::string> split_newline(const std::string& str)
{
    std::istringstream iss(str);
    std::vector<std::string> lines;
    std::string line;

    while (getline(iss, line)) {
        lines.push_back(line);
    }

    return lines;
}

std::vector<std::string> split_whitespace(const std::string& str)
{
    std::istringstream iss(str);
    std::vector<std::string> words;
    std::string word;

    while (iss >> word) {
        words.push_back(word);
    }

    return words;
}

std::string rdfile(const std::string& path)
{
    std::ifstream file_stream(path);
    std::stringstream buffer;
    buffer << file_stream.rdbuf();
    return buffer.str();
}

std::string remove_prefix(const std::string& str, const std::string& prefix)
{
    if (str.substr(0, prefix.size()) == prefix) {
        return str.substr(prefix.size());
    }
    return str;
}

std::string remove_suffix(const std::string& str, const std::string& suffix)
{
    if (str.size() >= suffix.size() &&
        str.substr(str.size() - suffix.size()) == suffix) {
        return str.substr(0, str.size() - suffix.size());
    }
    return str;
}

bool has_prefix(const std::string& str, const std::string& prefix)
{
    return str.size() >= prefix.size() &&
           str.compare(0, prefix.size(), prefix) == 0;
}

bool has_suffix(const std::string& str, const std::string& suffix)
{
    return str.size() >= suffix.size() &&
           str.compare(str.size() - suffix.size(), suffix.size(), suffix) == 0;
}

std::vector<std::string> split(const std::string& s,
                               const std::string& delimiter)
{
    std::vector<std::string> result;
    size_t start = 0;
    size_t end = s.find(delimiter);

    while (end != std::string::npos) {
        result.push_back(s.substr(start, end - start));
        start = end + delimiter.length();
        end = s.find(delimiter, start);
    }

    result.push_back(s.substr(start, end));
    return result;
}

std::vector<int> extract_integers(const std::string& str)
{
    std::istringstream iss(str);
    std::vector<int> numbers;
    int number;
    while (iss >> number) {
        numbers.push_back(number);
    }
    return numbers;
}

std::vector<long long> extract_lls(const std::string& str)
{
    std::istringstream iss(str);
    std::vector<long long> numbers;
    long long number;
    while (iss >> number) {
        numbers.push_back(number);
    }
    return numbers;
}

std::vector<unsigned long> extract_unsigned_longs(const std::string& str)
{
    std::istringstream iss(str);
    std::vector<unsigned long> numbers;
    unsigned long number;
    while (iss >> number) {
        numbers.push_back(number);
    }
    return numbers;
}

// BeforeOrSame returns the substring before the first occurrence of sep in s.
// If sep is not found, it returns the original string.
std::string before_or_same(const std::string& s, const std::string& sep)
{
    size_t index = s.find(sep);
    if (index != std::string::npos) {
        return s.substr(0, index);
    }
    return s;
}

// AfterOrSame returns the substring after the first occurrence of sep in s.
// If sep is not found, it returns the original string.
std::string after_or_same(const std::string& s, const std::string& sep)
{
    size_t index = s.find(sep);
    if (index != std::string::npos) {
        return s.substr(index + sep.length());
    }
    return s;
}
