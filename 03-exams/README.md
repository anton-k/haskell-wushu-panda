# Exam engine

For this project we will write generic tool to pass an examine
or take a psychological test. The user answers series of questions
at the end user sees the scores or final result. It can be the final mark
or sort of category that user belongs to. 

So let's spawn a new stack project and dive into the task.

```
stack new exam-engine
```

It's going to be the CLI application again. User will go over a list of
single choice questions and get results at the end.

## Exercises

* Add logging of the answers to the `ExamResult`. So that we can know not
  only final result but also answers.

* Can we limit our exam by time? Often we have only limited amount of time to pass the exam.
    Add timer and time stamps of when exam has started and when it was finished in the `ExamResult`.
    And force exit on end of time.    

* Can we devise some method of summarizing the results?
    Like some bins are antogonist of each other and we can estimate 
      a single result based on proportion of them. 
      Ad also we can pick up one of the bins if it's more prominent then the other.
      Implement result estimation system and make it flexible and configurable.
      
* Creative task. We can go one step further and turn our exam engine
  to the text game engine. Only in the text game we traverse not the list of questions
  but the graph of states. The gave is described as graph and on each move 
  our choice can lead to the different state. Also it will be interesting to add
  health parameters to the player and transition will depend not only on the 
  choice vut also on the current health-parameter and it's value.

