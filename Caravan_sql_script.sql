create view target_join as 
Select ROW_NUMBER() over() as num, target.* from target;

create view testdata_join as 
Select ROW_NUMBER() over() as num, testdata.* from testdata;

create table joined as 
SELECT testdata_join.*, target_join.caravan
from testdata_join
join target_join
on testdata_join.num = target_join.num;

alter table joined
drop column num;

Create table dataset as
Select * from trainingdata
union all
select * from joined;


SELECT DISTINCT CARAVAN , count(caravan) as Distribution
from dataset
GROUP BY caravan;


Select count(*) as Boat_caravan from dataset
where APLEZIER >=1 and CARAVAN=1;


