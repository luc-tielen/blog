build:
	stack run

deploy:
	./deploy.sh

clean:
	stack clean
	rm -rf .shake .stack-work/

.PHONY: build deploy clean
