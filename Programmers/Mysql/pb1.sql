-- 코드를 입력하세요
SELECT count(name) 
FROM (SELECT distinct name from animal_INS) T 
WHERE name is NOT NULL