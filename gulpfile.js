// Import packages
const gulp = require('gulp')
const elm = require('gulp-elm')
const watch = require('gulp-watch')
const shell = require('gulp-shell')
const plumber = require('gulp-plumber')
const webserver = require('gulp-webserver')

// Initialize gulp-elm
gulp.task('elm-init', elm.init)

// Make elm task
gulp.task('make', ['elm-init'], () => {
  return gulp.src('src/*.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest('dist/'))
})

// Test
gulp.task('test', ['elm-init'], () => {
  return gulp.src('tests/*.elm')
    .pipe(plumber())
    .pipe(elm())
    .pipe(gulp.dest('tmp/'))
    .pipe(shell(
      [ 'echo start elm-test build'
      , 'sh ./elm-stuff/packages/laszlopandy/elm-console/1.1.0/elm-io.sh tmp/Main.js tmp/test.js'
      , 'node tmp/test.js' ]
    ))
})

// Watch for changes and run test automatically
gulp.task('watch', function() {
  gulp.src('src/')
    .pipe(webserver({
      livereload: true,
      directoryListing: true,
      open: true
    }))
  gulp.start('make')
  gulp.start('test')
  gulp.watch('src/**', ['make'])
  gulp.watch('tests/**', ['test'])
})

// By default run tests and then starts
// watching for changes
gulp.task('default', ['watch'])
