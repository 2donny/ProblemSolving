function solution(board, moves) {
    var answer = 0;
    let stack = [];
    let stackCnt = 0;
    
    let new_arr = [];
    let arr = [];
    
    for(let i = 0; i < board.length; i++) {
        for (let j = 0; j < board.length; j++) {
            arr.push(board[j][i]);  //j, i를 각각 행, 열로 생각하면 편함.
        }       
        new_arr.push(arr);
        arr = [];
    }
    
    console.log("new_arr : ", new_arr);

    for(let i = 0; i < moves.length; i++) {
        let col_index = moves[i] - 1; //꺼내야할 board의 인덱스
        let top = undefined;
        for( ; ; ) {  // 0을 무시하기 위한 루프
            if(new_arr[col_index].length === 0) 
                break;
            top = new_arr[col_index].shift(); // 없는 상태로 pop하니까 top에는 undefined가 들어가고, 이 루프 
            if(top === 0) //계속 0이면 전부 pop 시킴.
                continue;
            else
                break;
        }
        if(top === undefined) //끝까지 갔었으면, 그 col에서 인행 꺼내기 실패
            continue;
         // 추가
        stack.push(top);
        stackCnt = stackCnt + 1;
        console.log('arr : ', new_arr);
        console.log('stack : ', stack, 'stackCnt : ', stackCnt, '\n');

        if(stackCnt >= 2) { //추가 한 후에 stack 확인
            if(stack[stackCnt-1] === stack[stackCnt-2]) { // 같은 게 있다면 없애야함.
                stack.pop();
                stack.pop();
                stackCnt = stackCnt - 2;
                answer = answer + 2;
            }else {
                continue;
            }
        }
    }
    console.log(answer);
    return answer;
}

let ans = solution([[0, 0, 0, 0, 0], [0, 0, 1, 0, 3], [0, 2, 5, 0, 1], [4, 2, 4, 4, 2], [3, 5, 1, 3, 1]], [1, 5, 3, 5, 1, 2, 5, 1, 4, 3]);
console.log('ans : ', ans);

