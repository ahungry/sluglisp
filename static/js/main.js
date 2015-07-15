function package_details(url)
{
    $.ajax({
        data: '',
        type: 'get',
        url: url,
        success: function(res)
        {
            $('#package-details').html(res);
        }
    });
}

$(document).ready(function() {
    var last_search = window.location.href.replace(/.*?#/, '');
    if (/^\/saved/.test(last_search))
    {
        package_details(last_search.replace(/^\/saved/, ''));
    }

    $('.package-name').click(function() {
        window.location.href='#/saved' + $(this).attr('href');
        package_details($(this).attr('href'));
        return false;
    });
});
