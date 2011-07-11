package ru.apeon.comtec

import ru.apeon.core.sql.{AsaSqlDialect, SqlTable}

class ComtecAsaSqlDialect extends AsaSqlDialect {
  override def createTableStatement(table: SqlTable) =
"""%2$s
create TRIGGER "newid" before insert order 100 on %1$s
referencing new as new_name
for each row
begin
	declare ll_id Int;
	set ll_id=new_name.id;
	if ll_id is null then
		set new_name.id=get_next_id('%3$s','id');
	end if
end;
""".format(table, super.createTableStatement(table), table.name)

  override def lastIdentityExpression(table : SqlTable) = "get_last_identity('" + table.name + "')"
}

