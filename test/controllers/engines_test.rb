require 'test_helper'

class EnginesTest < ActionDispatch::IntegrationTest
  test 'pathpuz engine' do
    # TODO: I guess this doesn't work for mounted engines?
    # assert_routing('/pathpuz', controller: 'pathpuz#welcome', action: 'index')
    get pathpuz.root_url
    assert_response :success
  end
end
