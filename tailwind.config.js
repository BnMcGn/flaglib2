/** @type {import('tailwindcss').Config} */
module.exports = {
    content: ["./resources/public/cljs-out/**/*.js",
              "./src/warflagger-web/*.lisp"],
  theme: {
      extend: {},
      fontFamily: {
          sans: ['sans-serif'],
          serif: ['helvetica', 'arial']
      }
  },
  plugins: [],
}
