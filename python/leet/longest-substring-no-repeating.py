#!/usr/bin/env python3
class Solution(object):
    def lengthOfLongestSubstring(self, s):
        """
        :type s: str
        :rtype: int
        """
        longest = 0
        current = 0
        map = {}
        i = 0
        j = i
        # e.g. 'pbwwcpa' 'abba'
        #       0123456   0123
        while i < len(s):
            while j < len(s):
                char = s[j]
                index = map.get(char)
                if index is not None:
                    if current > longest:
                        longest = current
                    # set current to correct value
                    current = j - index
                    # remove obsolete entries from map
                    for k in range(i, index + 1):
                        del map[s[k]]
                    # advance i
                    i = index + 1
                    map[char] = j
                else:
                    map[char] = j
                    current += 1
                j += 1
            if current > longest:
                longest = current
            break
        return longest



s = Solution()
answer = s.lengthOfLongestSubstring('abcabcbb')
print('answer:', answer)