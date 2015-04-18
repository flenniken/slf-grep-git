# slf-grep-git
Grep for text in source files.

`slf-grep-git` is an emacs command which searches for text in your                                                     
source files. You select some text and press a key and the source                                                      
files matching are appended to a results buffer.                                                                       
                                                                                                                       
The command runs the shell commands:                                                                                   
                                                                                                                       
    git ls-files | grep -v -f ignore.txt | xargs grep -n                                                               
                                                                                                                       
The search text comes from the current selection, or when there is no                                                  
selection, you type it in. The typed in text is treated as a                                                           
regular expression and the selected text is treated as a fixed                                                         
string.                                                                                                                
                                                                                                                       
The matching lines are appended to a buffer called slf-grep-[number].                                                     
                                                                                                                       
The source files to search come from the `git ls-files`                                                                
command. The ls-files command is run from the git root directory                                                       
so by default all source files of the project are searched.                                                            
                                                                                                                       
The command finds the git root directory by looking relative to                                                        
the current buffer. You can have multiple git projects on your                                                         
machine and the correct set of files will be searched. Each                                                            
project gets its own results buffer.

If you have files in your project that you do not want to search,                                                      
add them to the ignore.txt file. The files in ignore.txt are                                                           
skipped. Enter one file or pattern per line. The ignore.txt file                                                       
in the git root folder is used when it exists, else the one in                                                         
the home folder is used.                                                                                               
                                                                                                                       
If the number of matching lines is more than                                                                           
`slf-grep-max-lines` (400), the lines are not appended to the                                                          
results buffer. If the number of characters is greater than                                                            
`slf-grep-max-chars` (200), the command is ignored. You can                                                            
configure these values if you want.                                                                                    
                                                                                                                       

Install:
========

To install grab the `slf-grep-git.el` file and put it on your emacs load
path and add a line to load it in your .emacs file:

    (load-file "~/emacs/slf-grep-git.el")
    (global-set-key "\C- " 'slf-grep-git) ; Ctrl Space
    
