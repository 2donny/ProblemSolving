function solution(array, commands) {
    
    return commands.map(el => {
        let [i, j, k] = el; // commands를 인덱스로 바꾸기.
        const arr = array.slice(--i, j); // 자른 새로운 배열 리턴.
        const new_arr = arr.sort((a, b) => a - b); // compareFunction을 작성하지 않으면, 문자열 유니코드를 기준으로 정렬하기 때문에 에러 발생.
        return new_arr[--k];
    })
}