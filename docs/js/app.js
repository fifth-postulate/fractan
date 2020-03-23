(function(){
  let slideshow = remark.create({
    sourceUrl: 'presentation.md'
  });
  slideshow.on('showSlide', once(function(){
    let samples = Array.prototype.slice.call(document.getElementsByTagName('div'))
      .filter(function(sample){ return sample.classList.contains('fractan')});
    samples.forEach(function(sample){
      let source = sample.getElementsByTagName('p')[0].innerText;
      let description = JSON.parse(source);
      let flags = { description: description };
      let app = Elm.Fractan.init({
        node: sample,
        flags: flags
      });
    });
  }));

  function once(callback){
    var called = false;
    return function(){
      if (!called) {
        called = true;
        callback();
      }
    }
  }

 
  // Setup MathJax
  MathJax.Hub.Config({
    tex2jax: {
      skipTags: ['script', 'noscript', 'style', 'textarea', 'pre']
    }
  });
  MathJax.Hub.Configured();
})();
