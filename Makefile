.PHONY: test lint all

all:	test lint

test:
	clojure -M:test -m kaocha.runner --reporter kaocha.report/dots "$@"

lint:
	clj-kondo --lint src/juxt --lint test/juxt
