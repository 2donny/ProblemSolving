def solution(numbers):
    final_dict = {}
    for i in range(len(numbers)-1):
        for j in range(i+1, len(numbers)):
            val = numbers[i] + numbers[j] # 서로 다른 인덱스의 값을 더한 후에
            if val not in final_dict: # 더한 값이 dictionary에 없을 때에만 append한다.
                final_dict.update({val: 1})

    return sorted(final_dict)
