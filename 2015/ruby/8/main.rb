require 'stringio'

# count_chars calculates the number of characters in the code and the
# number of characters in the string representation, excluding escape sequences.
def count_chars(s, code_chars, string_chars)
  code_chars += s.length

  i = 1
  while i < s.length - 1  # Skip first and last quote
    if s[i] == '\\'
      if i + 1 < s.length - 1
        case s[i + 1]
        when '\\', '"'
          string_chars += 1
          i += 1
        when 'x'
          if i + 3 < s.length - 1
            string_chars += 1
            i += 3
          end
        else
          string_chars += 1
        end
      end
    else
      string_chars += 1
    end
    i += 1
  end

  return code_chars, string_chars
end

# encode_string returns an encoded version of the input string and its length.
def encode_string(s)
  b = StringIO.new
  b.write("\"")

  s.each_char do |ch|
    if ch == '"' || ch == '\\'
      b.write('\\')
    end
    b.write(ch)
  end

  b.write("\"")
  return b.string, b.string.length
end

def main
  if ARGV.length < 1
    STDERR.puts "missing input file"
    exit 1
  end

  begin
    f = File.open(ARGV[0], "r")
  rescue => e
    STDERR.puts "cannot open file: #{e}"
    exit 1
  end

  total_code_chars = 0
  total_string_chars = 0
  total_orig_chars = 0
  total_enc_chars = 0

  f.each_line do |line|
    line.strip!

    # Part 1: Count characters
    total_code_chars, total_string_chars = count_chars(line, total_code_chars, total_string_chars)

    # Part 2: Encode string
    total_orig_chars += line.length

    _, enc_len = encode_string(line)
    total_enc_chars += enc_len
  end

  f.close

  puts "Part 1: #{total_code_chars - total_string_chars}"
  puts "Part 2: #{total_enc_chars - total_orig_chars}"
end

# Run the main function if this script is executed
if __FILE__ == $0
  main
end
