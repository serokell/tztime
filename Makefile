# SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io>
#
# SPDX-License-Identifier: MPL-2.0

.PHONY: tztime test haddock haddock-no-deps stylish lint clean all doctest test-unit

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=tztime

tztime:
	$(MAKE_PACKAGE) dev
test:
	make test-unit
	make doctest
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean

stylish:
	find . -name '.stack-work' -prune -a -name 'dist-newstyle' -prune -o -name '*.hs' | xargs stylish-haskell -i

lint:
	hlint .


####################################
# Individual test suites

doctest:
	stack test --fast tztime:test:tztime-doctest --test-arguments "$(DOCTEST_ARGUMENTS)"

test-unit:
	$(MAKEU) test PACKAGE="tztime:test:tztime-test"
