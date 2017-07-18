# Tested in python 2.7

# Initial effort. Hope to tidy up for future use


import subprocess, sys

class tester:
    """Framework for application testing"""

    # Log Levels
    LOG_ALL = 0
    LOG_WARN = 1
    LOG_ERR = 2
    

    # Terminal Format Strings
    RESTORE = "\033[0m"
    GREEN = "\033[32m"
    RED = "\033[31m"
    BOLD = "\033[1m"
    INDENT = "    "
    BLUE_BG = "\033[44m"

    # Test Settings    
    config = {
        "cmd" : [],
        "name" : "No Application",
        "log_level" : LOG_ALL
        }

    passed = 0
    failed = 0
    
    def log_all(self, msg):
        if(self.config["log_level"] > self.LOG_ALL):
            return
        print self.INDENT + msg
        
    def log_warn(self, msg):
        if(self.config["log_level"] > self.LOG_WARN):
            return
        print self.INDENT + msg

    def log_err(self, msg):
        print >> sys.stderr, self.INDENT + self.RED + "FAIL: " + self.RESTORE + msg
        

    def run_test(self, test_struct):
        """Run the test. The input is a dictionary with the following
        arguments.
        
        name: Test name

        args: The command arguments to be added to the application
        (Unit Under Test), as an array of strings.

        input: string of data for input. eg "out" or tester.file("output.txt")

        stdout_check: function which takes a single argument: the
        stdout stream. Return False if failed test, or True

        stderr_check: The same thing but for stderr
        
        reference_app: application name to be used as a reference. If there are
        no output checks, the output is checked verbatim. If output
        checks exist, then they should be functions taking two
        inputs. The first will be the reference application, the
        second the UUT output

        If input is undefined it is assumed empty. If the checks are
        undefined, they are not checked.

        """

        error = False;
        
        # Sort out args to run.
        args = self.config["cmd"] + test_struct["args"]

        if("input" in test_struct):
            std_in = test_struct["input"]
        else:
            std_in = "" 

        print  self.BOLD + "Running Test: " + self.config["name"] + "::" + test_struct["name"] + self.RESTORE
        
        self.log_all("Command Line Arguments: " + str(args))

        # Start subprocess
        p = subprocess.Popen(args, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        stderr=subprocess.PIPE)

        self.log_all("PID: " + str(p.pid))
        
        (std_out, std_err) = p.communicate(std_in)

        ret_val = p.returncode
        if(ret_val != 0):
            self.log_err("Non-zero return code: " + str(ret_val))
            error = True
        else:
            self.log_all("Process under test exited returning 0")

        ################### STDOUT ######################

        if("reference_app" in test_struct):
            # Reference app available

            # Run reference
            ref_args = test_struct["reference_app"] + test_struct["args"]
            q = subprocess.Popen(ref_args, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
            self.log_all("Reference PID: " + str(q.pid))
            (ref_std_out, ref_std_err) = q.communicate(std_in)

            ret_val = q.returncode
            if(ret_val != 0):
                self.log_err("Reference had non-zero return code: " + str(ret_val))
                error = True
            else:
                self.log_all("Reference exited returning 0")
            
            if("stdout_check" in test_struct):
                # Run check func if exists
                self.log_all("Using test function on reference")
                if(not test_struct["stdout_check"](ref_std_out, std_out)):
                    self.log_err("stdout failed test")
                    error = True
            else:
                # Test exact match
                self.log_all("Testing against reference output")
                if(std_out != ref_std_out):
                    self.log_err("stdout different from reference")
                    error = True

            if(error):
                print "--- Reference Output:"
                print ref_std_out
                
        else:
            # Run normal tests
            if("stdout_check" in test_struct):
                if(isinstance(test_struct["stdout_check"], basestring)):
                    # String check
                    self.log_all("stdout is string check")
                    if(test_struct["stdout_check"] != std_out):
                        self.log_err("stdout does not match test string")
                        error = True
                else:
                    # Function check
                    if(not test_struct["stdout_check"](std_out)):
                        self.log_err("stdout failed test")
                        error = True
            else:
                # Lack of check
                self.log_warn("No stdout check")

                        
        if(error):
            print "--- Output:"
            print std_out

        ################### STD ERR #########################
        
        if("reference_app" in test_struct):
            # Reference app available
            
            if("stderr_check" in test_struct):
                # Run check func if exists
                self.log_all("")
                if(not test_struct["stderr_check"](ref_std_err, std_err)):
                    self.log_err("stdout failed test")
                    error = True
            else:
                # Test exact match
                if(std_err != ref_std_err):
                    self.log_err("stdout different from reference")
                    error = True

            if(error):
                print "--- Reference Error Output:"
                print ref_std_err
            
        else:
            # Normal tests
            if("stderr_check" in test_struct):
                if(isinstance(test_struct["stderr_check"], basestring)):
                    # String comparison
                    self.log_all("stderr is string check")
                    if(test_struct["stderr_check"] != std_out):
                        self.log_err("stderr does not match test string")
                        error = True
                else:
                    # Test func
                    if(not test_struct["stderr_check"](std_out)):
                        self.log_err("stderr failed test")
                        error = True                
            else:
                # Lack of test
                self.log_warn("No stderr check")

        
        if(error):
            print "--- Error Output:"
            print std_err


        # Test summary
        if(error):
            print self.RED + "------------ FAILED" + self.RESTORE
            self.failed += 1
        else:
            print self.GREEN + "------------ PASSED" + self.RESTORE
            self.passed += 1


    
    def final_print(self):
        """ Print the summary of the run of tests. """
        print self.BOLD + self.BLUE_BG + self.INDENT + str(self.passed) + " of " + str(self.passed + self.failed) + " passed" + self.INDENT + self.RESTORE

        
    def is_one_of (self, string_array):
        """" Returns a function that checks that its input is one of the given strings """
        def internal_func(data):
            for i in range(0, len(string_array)):
                if(data == string_array[i]):
                    return True;

            return False;

        return internal_func
    
                
    def inputFile(self, filename):
        """Reads input from a file (as a test input,
        or for output check from a known answer)"""
        file = open(filename, 'r')
        return file.read()

    def outputContains(self, strn):
        def v (s):
            return s.find(strn) >= 0

        return v
