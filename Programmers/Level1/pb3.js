function solution(answers) {
    let cnt1 = 0      // 맞은 갯수
    let cnt2 = 0
    let cnt3 = 0
    const p1 = [1, 2, 3, 4, 5]; // 1번의 패턴
    const p2 = [2, 1, 2, 3, 2, 4, 2, 5];
    const p3 = [3, 3, 1, 1, 2, 2, 4, 4, 5, 5];
    
    
    const a1 = []; // 1번이 찍은 정답
    const a2 = [];
    const a3 = [];
    
    const len = answers.length;
    for (let i = 0; i < len; i++) {
        a1.push(p1[i%5]);
        a2.push(p2[i%8]);
        a3.push(p3[i%10]);
    }
 
    for (let i = 0; i < len; i++) {
        if(answers[i] === a1[i])
            cnt1++;
        if(answers[i] === a2[i])
            cnt2++;
        if(answers[i] === a3[i])
            cnt3++;
    }
    
    const answer = [];
    const max = Math.max(cnt1, cnt2, cnt3);
    if(max === cnt1)
        answer.push(1);
    if(max === cnt2)
        answer.push(2);
    if(max === cnt3)
        answer.push(3);
        
    return answer;
}