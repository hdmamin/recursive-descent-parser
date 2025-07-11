package main

import (
	"fmt"
	"os"
	"github.com/codecrafters-io/interpreter-tester/internal"
)

func main() {
	// 1. Determine the correct TESTER_DIR.
	// This assumes you run this Go program from the 'tests/' directory.
	currentDir, err := os.Getwd()
	if err != nil {
		fmt.Printf("Error getting current working directory: %v\n", err)
		return
	}

	// Since this file is in 'tests/', currentDir should already be 'tests/'.
	// So, TESTER_DIR is simply the current directory.
	testerDir := currentDir
	os.Setenv("TESTER_DIR", testerDir)
	fmt.Printf("TESTER_DIR set to: %s\n", os.Getenv("TESTER_DIR"))

	// 2. Define the stage identifier you want to test.
	stageIdentifier := "s1" // For Stage #401

	fmt.Printf("Attempting to load test cases for stage identifier: %s\n", stageIdentifier)

	// 3. Call the GetTestCasesForCurrentStage function.
	testCases := internal.GetTestCasesForCurrentStage(stageIdentifier)

	// 4. Print the results if successful.
	fmt.Printf("Successfully loaded %d test cases for stage '%s'.\n", len(testCases), stageIdentifier)
}
