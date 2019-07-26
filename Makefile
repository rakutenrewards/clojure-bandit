.PHONY: test-jenkins
test-jenkins:
	lein with-profile ci test
