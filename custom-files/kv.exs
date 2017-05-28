# kv.exs
defmodule KV do  # KeyValue
  @doc "dictionar cheie-valoare"

  def start_link do
    # modulul Task aduce error reporting imbunatatit
    Task.start_link(fn -> loop(%{}) end) # incepem cu un dictionar gol
  end

  defp loop(map) do
    receive do
      {:get, key, caller} ->
        send caller, Map.get(map, key)  # raspundem apelantului
        loop(map)                       # continuam sa asteptam mesaje
      {:put, key, value} ->
        loop(Map.put(map, key, value))  # actualizam modificand argumentul
    end
  end
end
