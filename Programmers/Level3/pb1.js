function solution(n, costs) {
    let count = 0; 
    costs = costs.sort((a, b) => a[2] - b[2]);
    let islands = Array.from({length: n}, (v, i) => i);
    for(let i = 0; i < costs.length; i++){
        const cost = costs[i];
        const from = islands[cost[0]];
        const to = islands[cost[1]];
        if(from !== to){
            islands = islands.map(island => {
                if(island === to){ return from}
                return island;
            })
            count += cost[2];
        }
        if(new Set(islands).length === 1){
            break;
        }
    };
    return count;
}