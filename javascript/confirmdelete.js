$(document).ready(function()
{
    $('.confirm-delete').click(function()
    {
        return confirm('Are you sure you want to delete "' + $(this).attr('title') + '" from the Link Manager?');
    });
});