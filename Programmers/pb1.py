def solution(participant, completion):
    c_set = {}   # completion 집합
    for i in completion:
        if i not in c_set:
            c_set[i] = 1
            continue
        c_set[i] += 1
    print("c : ", c_set)

    for k in participant:
        if k in c_set:
            if c_set[k] < 1: # 이게 정답이 됨
                answer = k    
                break
            else:
                c_set[k] -= 1
        else: 
            answer = k   
            break 
    
    return answer

ans = solution(['marina', 'josipa', 'nikola', 'vinko', 'filipa'], ['josipa', 'filipa', 'marina', 'nikola'])
print(ans)