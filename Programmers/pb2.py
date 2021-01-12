def solution(participant, completion):
    dic_hash = {indx : 0 for indx in participant}

    for i in completion:
        if i in dic_hash:
            dic_hash[i] += 1

    for j in participant:
        if j in dic_hash:
            dic_hash[j] -= 1
            if dic_hash[j] < 0:
                answer = j
        else:
            answer = j
    return answer