function solution(clothes) {
    const map = new Map();

    for (let i = 0; i < clothes.length; i++) {
        if (!map.has(clothes[i][1])) {
            map.set(clothes[i][1], 1 + 1)
            continue
        }
        map.set(clothes[i][1], map.get(clothes[i][1]) + 1)
    }

    return Array.from(map).reduce((acc, el) => acc * el[1], 1) - 1;
}

console.log(solution([
    ["yellowhat", "headgear"],
    ["bluesunglasses", "eyewear"],
    ["green_turban", "headgear"]
]));