class Solution(object):
    def minCostClimbingStairs(self, cost):
        """
        :type cost: List[int]
        :rtype: int
        """
        min_costs = self.min_costs(cost)
        return min(min_costs[0], min_costs[1])

    def min_costs(self, cost):
        length = len(cost)
        if length <= 2:
            if length == 1:
                return cost[0], 0
            return cost[1], cost[0]
        sub = self.min_costs(cost[0:length-1])
        final_sub_cost = sub[0]
        penultimate_sub_cost = sub[1]
        final_cost = min(final_sub_cost + cost[-1],
                         penultimate_sub_cost + cost[-1])
        penultimate_cost = final_sub_cost
        return final_cost, penultimate_cost

s = Solution()
result = s.minCostClimbingStairs([1, 100, 1, 1, 1, 100, 1, 1, 100, 1])
result = s.minCostClimbingStairs([1, 1, 1, 100, 1])
result = s.minCostClimbingStairs([1, 1, 1, 100])
result = s.minCostClimbingStairs([1, 1, 1])
print('result:', result)
