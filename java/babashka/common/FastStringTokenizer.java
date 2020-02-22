package babashka.common;

import java.util.Arrays;

public class FastStringTokenizer {
  private static ThreadLocal<String[]> tempArray = new ThreadLocal<String[]>();
  static {
    String[] buff = new String[10];
    tempArray.set(buff);
  }
  private static String[] EMPTY = new String[0];

  /**
   * Fast implementation of a str tokenize method. Both StringTokenizer and String.split() have
   * drawbacks (i.e. missing tokens at the end of line if a line is terminated by
   * a delimiter, having to use sub-par performing regex implementations, having to apply
   * the regex multiple times to retrieve all the tokens, use complex strings as delimiters
   * when we know we are dealing with tabs etc etc ) and do things way too fancy for this situation.
   *
   * @param str String to tokenize
   * @param delimiter Delimiter character
   * @return the resulting str array
   */
  public static String[] tokenize(String str, char delimiter) {
    String[] temp = tempArray.get(); // new String[str.length() + 1];

    if (str.length() == 0) return EMPTY;

    int tempLength = str.length() + 1;
    if (temp == null || temp.length < tempLength) {
      temp = new String[tempLength];
      tempArray.set(temp);
    }

    int wordCount = 0;
    int i = 0;
    int j = str.indexOf(delimiter);

    while (j >= 0) {
      temp[wordCount++] = str.substring(i, j);
      i = j + 1;
      j = str.indexOf(delimiter, i);
    }

    temp[wordCount++] = str.substring(i);

    String[] result = new String[wordCount];
    System.arraycopy(temp, 0, result, 0, wordCount);

    return result;
  }

  public static String[] tokenize2(String str, char delimiter) {
    if (str == null || str.length() == 0) return EMPTY;

    String[] temp = tempArray.get(); // new String[str.length() + 1];
//    final int tempLength = str.length() + 1;
//    if (temp == null || temp.length < tempLength) {
//      temp = new String[tempLength];
//      tempArray.set(temp);
//    }

    final char[] chars = str.toCharArray();
    final int    len   = chars.length;
    int    i     = 0;
    int    k     = 0;
    int    s     = 0;
    for (; i < len; i++) {
      char c = chars[i];
      if (c == delimiter) {
        temp[k++] = new String(chars, s, i-s); // String(chars, s, i - s);
        s = i + 1;
      }
    }

    temp[k++] = new String(chars, s, i-s);

    return Arrays.copyOfRange(temp, 0, k);
  }

  public static String[] tokenize3(String str, char delimiter) {
    if (str == null || str.length() == 0) return EMPTY;

    String[] temp = tempArray.get(); // new String[str.length() + 1];
    final int tempLength = str.length() + 1;
    if (temp == null || temp.length < tempLength) {
      temp = new String[tempLength];
      tempArray.set(temp);
    }

    //final char[] chars = str.toCharArray();
    final int    len   = str.length(); //.length;
    int    i     = 0;
    int    k     = 0;
    int    s     = 0;
    for (; i < len; i++) {
      char c = str.charAt(i);
      if (c == delimiter) {
        temp[k++] = str.substring(s, i); // String(chars, s, i - s);
        s = i + 1;
      }
    }

    temp[k++] = str.substring(s, i);

    return Arrays.copyOfRange(temp, 0, k);
  }
}
