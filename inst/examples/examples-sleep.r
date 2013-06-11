
slow_mean <- mcSleep(f = mean, n = 1)
slow_mean(1)

slow_hello <- mcSleep(
	function () print("good morning! *yawn*"),
	n = 1)
slow_hello()
