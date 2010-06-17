$(function() {
    var nativeWind = window,
    wind = $(nativeWind),
    onloadmore = 'onloadmore',
    maybeLoadMore = function() {
        var loader = $('#lazy-loader').parent(),
        loadMore = loader.attr(onloadmore);
        if(loadMore) {
            var margin = 200,
            top = wind.scrollTop() - margin,
            windJQHeight = wind.height(),
            bottom = top + ((windJQHeight == 0) ? nativeWind.innerHeight : windJQHeight) + margin, //Height workaround for Chrome 5 and 6
            loaderTop = loader.offset().top;
            if(loaderTop > top && loaderTop < bottom) {
                eval(loadMore);
                loader.removeAttr(onloadmore);
            }
        }
    };
    wind.bind('scroll resize', maybeLoadMore);
    maybeLoadMore();
});
