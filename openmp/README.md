# OpenMPを使った数値微分ルーチンの追加について

## OpenMPディレクティブ文の挿入

* メモリ共有のマルチスレッド並列処理をもちいて、
プログラムの実行を高速化する
* 通常は1CPUコアごとに1スレッドが立ち上がるように実行する

* OpenMP版のコードは絶対にログインノードで走らせない！

### parallel文

```
!$omp parallel default(shared) private(k, ix, iy, iz) reduction(+:g)
  do k=1,10
    中略
  end do
!$omp end parallel
```

* 外側の`k`ループの前後に`!$omp parallel`ディレクティブを挿入する
* `!$omp parallel`と`!$omp end parallel`の間は複数CPUコアを使ったマルチスレッド実行が可能になる
* `default`, `private`, `reduction`は内部変数の共有・非共有を指示する。
ほとんどの変数はスレッド間で共有(share)するが、ループ変数`ix,iy,iz`などはスレッドごとに独立に管理する(private)、計算結果`g`は各スレッドことに独立に計算したのち最後に集計する(reduction)。


### do文

```
    !$omp do collapse(2) 
    do iz=1,nzmax
      do iy=3*nymax/4+1,nymax
        do ix=1,nxmax
            中略
        end do
      end do
    end do
    !$omp end do
```

* `ix`, `iy`, `iz`のループの外側に、`do`ディレクティブを挿入、
ループの各要素の計算を複数のスレッドで分散処理させる。
* `collapse(2)`は外側の２つのループ(`ix`, `iy`)の並列化を指示。
* ループ範囲から抜ける時`!$omp end do`を入れる。

### スレッド数計測
* 実際に動いているスレッド数を表示させる。OpenMPの`omp_num_threads`関数を使う。
標準出力へのアクセスが伴うため、`!$omp single`ディレクティブを入れておく。

