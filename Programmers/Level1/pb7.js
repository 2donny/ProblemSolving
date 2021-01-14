function solution(a, b) {
    const day = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    let total_day = 0;
    
    for(let i = 0; i < (a-1); i++) {
        total_day += day[i];
    }
    total_day += b;

    const date = total_day % 7;
    let answer = '';
    switch(date) {
        case 0: 
            answer = 'THU';
            break;
        case 1:
            answer = 'FRI';
            break;
        case 2:
            answer = 'SAT';
            break;
        case 3:
            answer = 'SUN';
            break;
        case 4:
            answer = 'MON';
            break;
        case 5:
            answer = 'TUE';
            break;
        case 6:
            answer = 'WED';
            break;
    }
    return answer;
}

// 더 괜찮은 풀이
function getDayName(a,b){
    var date = new Date(2016, (a - 1), b);
      return date.toString().slice(0, 3).toUpperCase();
}