// Import packages
const gulp = require('gulp')
const elm = require('gulp-elm')
const watch = require('gulp-watch')
const shell = require('gulp-shell')
const plumber = require('gulp-plumber')
const webserver = require('gulp-webserver')
const sass = require('gulp-sass')


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

// Sass
gulp.task('sass', () => {
  return gulp.src('src/**/*.scss')
    .pipe(plumber())
    .pipe(sass({outputStyle: 'compressed'}))
    .pipe(gulp.dest('dist/'))
});

// Watch for changes and run test automatically
gulp.task('watch', () => {
  gulp.src('.')
    .pipe(webserver({
      livereload: true,
      directoryListing: false,
      open: true
    }))
  gulp.start('make')
  gulp.start('test')
  gulp.start('sass')
  gulp.watch('src/**', ['make'])
  gulp.watch('tests/**', ['test'])
  gulp.watch('src/**/*.scss', ['sass']);
})

// By default run tests and then starts
// watching for changes
gulp.task('default', ['watch'])
