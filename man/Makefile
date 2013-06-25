TARGETS=man1/pg_migrate.1 man5/pg_migrate.5

all: $(TARGETS)

man1/pg_migrate.1: man1/pg_migrate.1.markdown
	@pandoc -s -o man1/pg_migrate.1 -t man -V title=pg_migrate -V section=1 man1/pg_migrate.1.markdown

man5/pg_migrate.5: man5/pg_migrate.5.markdown
	@pandoc -s -o man5/pg_migrate.5 -t man -V title=pg_migrate -V section=5 man5/pg_migrate.5.markdown

clean:
	rm -Rf man*/*.[0-9]
