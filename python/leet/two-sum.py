#!/usr/bin/env python3

class Solution(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        result = Solution.find_sum_sorted(nums, target)
        if result is None:
            return None
        return [Solution.find_index(nums, result[0]),
                Solution.find_index(nums, result[1], desc=True)]

    @staticmethod
    def find_sum_sorted(nums, target):
        sorted_nums = sorted(nums)
        count = len(nums)
        i = 0
        j = count - 1
        candidate = sorted_nums[i] + sorted_nums[j]
        while True:
            if candidate > target:
                j -= 1
                if j == i:
                    j -= 1
            elif candidate < target:
                i += 1
                if i == j:
                    i += 1
            else:
                return [sorted_nums[i], sorted_nums[j]]
            if i == count or j == -1:
                return None
            candidate = sorted_nums[i] + sorted_nums[j]

    @staticmethod
    def find_index(nums, target, desc=False):
        length = len(nums)
        i = 0 if not desc else length - 1
        while True:
            if target == nums[i]:
                return i
            i += (1 if not desc else - 1)
            if desc and i < 0 or i == length:
                return None

class Solution2(object):
    def twoSum(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        map = {}
        for i in range(0, len(nums)):
            numi = nums[i]
            map[numi] = i
            candidate = map.get(target - numi)
            if candidate is not None and candidate != i:
                return sorted([i, candidate])
        return None




