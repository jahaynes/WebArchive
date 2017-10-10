pipeline {
  agent any
  stages {
    stage('update cabal') {
      steps {
        sh 'cabal update'
      }
    }
    stage('install WebArchive') {
      steps {
        sh 'cabal install'
      }
    }
  }
}