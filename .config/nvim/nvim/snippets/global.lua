return {
  -- gql_spec C-j
  graphql_spec = {
    prefix = "gql_spec",
    body = [[describe ${1:mutation}, type: :graphql do
  let(:query) do
    <<~GRAPHQL
      mutation $1 {
        $0
      }
    GRAPHQL
  end

  let(:variables) do
    {

    }
  end
end]],
  },

  -- spec C-j
  spec = {
    prefix = "spec",
    body = [[require "rails_helper"

describe ${1:class} do
  subject { described_class.${2:method} }

  it "" do

  end
end]]
  },

  test = {
    prefix = "test",
    body = [[package ${1:package}

import (
    "testing"
)

func Test${2:Func}(t *testing.T) {
    for name, tt := range map[string]struct{

    }{
        "${3:testcase}": {

        }
    } {
        t.Run(name, func(t *testing.T) {

        })
    }
}]]
  },
}
