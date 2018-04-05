$('.arrow-up').click(function () {
$('html, body').animate({
    scrollTop: ($('.container-wrapper').offset().top)
},900);
});
