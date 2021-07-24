package dev.krusling.leetcode

object LC0003_LongestSubstringWithoutRepeatingCharacters {

  // Inspired by Laagi
  def lengthOfLongestSubstring(s: String): Int = {
    var best = 0;
    var seenIndices: Map[Char, Int] = Map.empty;
    var lp = 0

    for (rp <- 0 until s.length) {
      if (seenIndices contains s(rp)) {
        lp = Math.max(lp, seenIndices(s(rp)) + 1)
      }
      seenIndices += (s(rp) -> rp)
      best = Math.max(best, rp - lp + 1)
    }

    best
  }
}
