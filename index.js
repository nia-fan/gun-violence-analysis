
function action(index) {
    var elems = document.querySelectorAll(".container-md");
    for (var i = 0; i < elems.length; i++) {
      if(i === index && !elems[i].classList.contains("w3-animate-zoom")){
        elems[i].classList.add("w3-animate-zoom");
      }else{ 
        elems[i].classList.remove("w3-animate-zoom");

      }
    }
  }

function scrollFunction() {
  if (document.body.scrollTop > 20 || document.documentElement.scrollTop > 20) {
    mybutton.style.display = "block";
  } else {
    mybutton.style.display = "none";
  }
}
  
// When the user clicks on the button, scroll to the top of the document
function topFunction() {
  document.body.scrollTop = 0;
  document.documentElement.scrollTop = 0;
}

// Get the button
let mybutton = document.getElementById("backtopBtn");
  
 // When the user scrolls down 20px from the top of the document, show the button
window.onscroll = function() {scrollFunction()};
  