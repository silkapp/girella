-- psql -f setup-example-db.sh
drop database test;

create database test;

do
$body$
begin
   if not exists
      ( select *
        from   pg_catalog.pg_user
        where  usename = 'test'
      ) then
      create user test password 'test';
   end if;
end
$body$;

grant all on database test to test;

drop table if exists people;

create table people
( id uuid primary key
, name text not null
, age int not null
, gender text
);
