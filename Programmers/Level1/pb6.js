function solution(n, lost, reserve) {
    let answer_cnt = n - lost.length;
    
    // 깊은 복사로 새로운 배열 생성. 이유 => lost.forEach()에서 정상적으로 모든 원소를 콜백하기 위해.
    const new_lostArr = lost.slice(); 
    const new_reserveArr = reserve.slice();

    // 먼저 체육복을 잃어버린 사람중, 여벌 있는 사람 filtering 하기.
    lost.forEach((lostEl) => { 
        console.log('lostEl : ', lostEl);
        if(reserve.find((reserveEl) => reserveEl === lostEl)) { // 자기꺼 입으면 됨.
            answer_cnt++;
            const index_l = new_lostArr.indexOf(lostEl);
            const index_r = new_reserveArr.indexOf(lostEl);
            new_lostArr.splice(index_l, 1); // lost 배열 수정.
            new_reserveArr.splice(index_r, 1); // reserve 배열 수정.
        }
    });
    
    // 여기서부터는 lost, reserve 배열에 같은 값을 가지고 있는 원소는 없음.
    new_lostArr.forEach(stu => {
        for(const [index, res] of new_reserveArr.entries()) {
            if(res === stu-1 || res === stu+1) {
                answer_cnt++;
                new_reserveArr.splice(index, 1); // 기존 배열에 Side-Effect를 발생시킨다.
                break; // 빌려입었으니 for문 탈출
            }
        }
    })
    return answer_cnt;
}
