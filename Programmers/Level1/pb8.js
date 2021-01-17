function solution(s) {
    const leng = s.length;
    
    if(leng % 2 ) { // 홀수면
        let cnt = Math.floor(leng / 2);
        console.log(cnt)
        return s[cnt]
    }else { // 짝수면
        let cnt = leng / 2;
        return s[cnt-1] + s[cnt]
    }
}