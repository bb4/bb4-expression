# bb4-project-template
A trivial bb4 project that can be used as a template when creating new bb4 projects. 

## How to use it

1. First create a new bb4-\<new project\> repository in github with no files in it.
1. Then either
   * Create a [bare clone](https://help.github.com/articles/duplicating-a-repository/) and modify it, or
   * Manually copy the project files from a clone of bb4-project-template into a new clone of the empty bb4-\<new project\> 
 directory on the file system. Do not copy the iml file or the following directories:
     * .* (.git, .gradle, .idea) 
     * build
     * gradle
     * out
   
   I prefer the second approach because then it will not have the git history from bb4-project-template.
   
1. Lastly, open the new project in intellij by opening the build.gradle file. In the import dialog, select the option to use the gradle wrapper instead of specifying the location of gradle. Now you have a working project to start from. Just modify it as needed to create your new bb4 project.

     
 
