from operator import add, mul

def cat(a, b): return int(f"{a}{b}")

def solve(nums, ops):
    if len(nums) == 2:
        return nums[0] == nums[1]

    total, a, b, *rest = nums
    for op in ops:
        if solve([total, op(a, b)] + rest, ops):
            return total
    return 0

data = [list(map(int, line.replace(':','').split())) for line in open('bin/day7/input.txt')]
print(sum(solve(nums, ops=[add, mul]) for nums in data))
print(sum(solve(nums, ops=[add, mul, cat]) for nums in data))