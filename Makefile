.PHONY: test lint all

all:	test lint

test:
	clojure -M:test -m kaocha.runner --reporter kaocha.report/dots "$@"

test-watch:
	find . -name "*.clj" | entr make test

lint:
	clj-kondo --lint src/juxt --lint test/juxt
