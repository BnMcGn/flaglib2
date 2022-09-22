/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ['./resources/public/cljs-out/**/*.js',
              './src/warflagger-web/*.lisp',],
  theme: {
      extend: {
          spacing: {
              'neg2.5': '-.625rem',
          }},
      fontFamily: {
          sans: ['sans-serif'],
          serif: ['helvetica', 'arial']
      }
  },
  plugins: [require('tailwind-children'),],
}
