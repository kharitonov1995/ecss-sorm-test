// https://medium.com/@mccode/understanding-how-uid-and-gid-work-in-docker-containers-c37a01d01cf
// https://jenkins.io/doc/book/pipeline/jenkinsfile/
// https://jenkins.io/blog/2017/01/19/converting-conditional-to-pipeline/
// choices are a string of newline separated values
// https://issues.jenkins-ci.org/browse/JENKINS-41180
// https://docs.docker.com/registry/deploying/#copy-an-image-from-docker-hub-to-your-registry

pipeline {
    agent any
    options {
        disableConcurrentBuilds()
    }
    parameters {
        booleanParam(
            name : 'PUBLISH',
            defaultValue: true,
        )
        booleanParam(
            name : 'REBUILD_TOOLCHAIN',
            defaultValue: true,
        )
        booleanParam(
            name : '__e2k__',
            defaultValue: false,
        )
    }
    stages {
        stage('Building toolchain') {
            when {
                expression {
                    params.REBUILD_TOOLCHAIN == true
                }
            }
            steps {
                sh "make toolchain"
            }
        }
        stage('Compiling') {
            steps {
                sh "make clean"
                sh "make up"
                sh "make build"
            }
        }
        stage('Build packages') {
            steps {
                sh "rm ./*.deb ./*.changes ./*.build ./*.upload || true"
                sh "make deb"
                archive (includes: '*.deb')
                archive (includes: '*.changes')
                archive (includes: '*.build')
            }
        }
        stage('Publishing') {
            when {
                expression {
                    params.PUBLISH == true
                }
            }
            steps {
                sh "make publish"
            }
        }
    }
    post {
        success {
            script {
                currentBuild.displayName = readFile("buildinfo").trim()
                // currentBuild.description = "Version is:"
            }
        }
    }
}
